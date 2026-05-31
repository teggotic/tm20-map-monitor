{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
module MapMonitor.GridDB where

import Control.Lens
import Data.Sequence
import Data.Acid
import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.IxSet.Typed hiding (fromList)
import qualified Data.IxSet.Typed as IxSet
import Data.SafeCopy
import Data.Time
import Protolude hiding (maximum)
import qualified RIO.Text as Text
import MapMonitor.DB
import Data.Aeson.Types (ToJSON(toJSON), FromJSON (parseJSON))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Servant.API (FromHttpApiData(..), ToHttpApiData(..))

newtype PosixTS = PosixTS { unPosixTS :: UTCTime }
  deriving newtype (Show, Eq, Ord)

$(deriveSafeCopy 0 'base ''PosixTS)

instance ToJSON PosixTS where
  toJSON = toJSON . posixTsToInt

instance FromJSON PosixTS where
  parseJSON a = posixTsFromInt <$> parseJSON a

instance FromHttpApiData PosixTS where
  parseUrlPiece x = posixTsFromInt <$> parseUrlPiece x

instance ToHttpApiData PosixTS where
  toUrlPiece = toUrlPiece . posixTsToInt

posixTsFromInt :: Int -> PosixTS
posixTsFromInt = PosixTS . posixSecondsToUTCTime . fromIntegral
posixTsToInt :: PosixTS -> Int
posixTsToInt = floor . utcTimeToPOSIXSeconds . unPosixTS

data ChatMessage
  = ChatMessage
  { _cm_id :: Int
  , _cm_message :: Text
  , _cm_author :: Text
  , _cm_timestamp :: PosixTS
  }
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = Protolude.drop (Text.length "_cm_")} ''ChatMessage)
$(deriveSafeCopy 0 'base ''ChatMessage)

newtype ID = ID { unID :: Text }
  deriving (Eq, Show, Ord)
  deriving newtype (ToJSON, FromJSON, Hashable, FromHttpApiData, ToHttpApiData)

$(deriveSafeCopy 0 'base ''ID)

inactivityTimeout :: NominalDiffTime
inactivityTimeout = 30

data Grid
  = Grid
  { _bb_uuid :: !ID
  , _bb_name :: !Text
  , _bb_mapUids :: ![TrackUid]
  , _bb_size :: !Int
  , _bb_chatLog :: !(Seq ChatMessage)
  , _bb_authorUid :: !Text
  , _bb_createdAt :: !PosixTS
  , _bb_updatedAt :: !PosixTS
  }
  deriving (Generic, Show, Eq)

$(makeLenses ''Grid)

$(deriveJSON defaultOptions{fieldLabelModifier = Protolude.drop (Text.length "_bb_")} ''Grid)
$(deriveSafeCopy 0 'base ''Grid)

instance Ord Grid where
  compare = comparing _bb_uuid

newtype CreatedAt = CreatedAt UTCTime
  deriving (Eq, Show, Ord)

newtype GridAuthor = GridAuthor Text
  deriving (Eq, Show, Ord)

newtype InactiveAfter = InactiveAfter UTCTime
  deriving (Eq, Show, Ord)

type BBIndexes = '[ID, CreatedAt, GridAuthor]
type BBTable = IxSet BBIndexes Grid

instance IxSet.Indexable BBIndexes Grid where
  indices = ixList
    (ixFun $ \bb -> [_bb_uuid bb])
    (ixFun $ \bb -> [CreatedAt $ unPosixTS $ _bb_createdAt bb])
    (ixFun $ \bb -> [GridAuthor $ _bb_authorUid bb])

data BBTableUpdate
  = BBTUAddBoard Grid
  | BBTUAddChatMessage ID ChatMessage
  | BBTUUpdateMapPool ID [TrackUid]

$(deriveSafeCopy 0 'base ''BBTableUpdate)

addChatMessage :: Grid -> ChatMessage -> Grid
addChatMessage brd msg =
  let
    messages = case viewl (_bb_chatLog brd) of
      EmptyL -> Data.Sequence.fromList [(msg {_cm_id = 0})]
      (a Data.Sequence.:< _) -> Data.Sequence.take 100 $ (msg {_cm_id = _cm_id a + 1}) Data.Sequence.<| _bb_chatLog brd
  in brd { _bb_chatLog = messages }
  
getBoard :: ID -> Query BBTable (Maybe Grid)
getBoard id = asks $ getOne . getEQ id

getBoardUpdatedAfter :: ID -> PosixTS -> Query BBTable (Maybe Grid)
getBoardUpdatedAfter id after =
  asks $ (getOne . getEQ id) >=> (\x -> if _bb_updatedAt x > after then Just x else Nothing)

getLastBoards :: Query BBTable [Grid]
getLastBoards = do
  asks $ Protolude.take 10 . toDescList (Proxy @CreatedAt)

getBoardsByIds :: [ID] -> Query BBTable [Grid]
getBoardsByIds ids = do
  asks $ toDescList (Proxy @CreatedAt) . (@+ ids)

getBoardsByUser :: Text -> Int -> Query BBTable [Grid]
getBoardsByUser authorUid n = do
  asks $ \db -> Protolude.take n $ toDescList (Proxy @CreatedAt) $ db @= GridAuthor authorUid

updateBBTable :: UTCTime -> BBTableUpdate -> Update BBTable ()
updateBBTable _   (BBTUAddBoard board) = do
  modify $ updateIx (_bb_uuid board) board
updateBBTable now (BBTUAddChatMessage gId msg) = do
  gets (getOne . getEQ gId)
    >>= \case
      Nothing -> pass
      Just board -> do
        modify $ updateIx gId ((addChatMessage board msg) {_bb_updatedAt = PosixTS now}) 
updateBBTable now (BBTUUpdateMapPool gId mapUids) = do
  gets (getOne . getEQ gId)
    >>= \case
      Nothing -> pass
      Just board -> do
        modify $ updateIx gId $
          board { _bb_mapUids = mapUids
                , _bb_updatedAt = PosixTS now
                } 

$(makeAcidic ''BBTable ['getBoard, 'updateBBTable, 'getLastBoards, 'getBoardUpdatedAfter, 'getBoardsByIds, 'getBoardsByUser])
