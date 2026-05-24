{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
module MapMonitor.GridDB where

import Control.Lens
import Data.Sequence
import Data.Acid
import Data.Foldable1
import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.IxSet.Typed hiding (fromList)
import qualified Data.IxSet.Typed as IxSet
import Data.SafeCopy
import Data.Time
import Protolude hiding (maximum)
import qualified Data.Map as Map
import qualified RIO.Text as Text
import MapMonitor.DB
import Data.Aeson.Types (ToJSON(toJSON), FromJSON (parseJSON))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Fixed (Pico)
import Servant.API (FromHttpApiData(..))

newtype PosixTS = PosixTS { unPosixTS :: UTCTime }
  deriving newtype (Show, Eq, Ord)

$(deriveSafeCopy 0 'base ''PosixTS)

instance ToJSON PosixTS where
  toJSON = toJSON @Int . floor . utcTimeToPOSIXSeconds . unPosixTS

instance FromJSON PosixTS where
  parseJSON a = PosixTS . posixSecondsToUTCTime . fromIntegral <$> parseJSON @Int a

instance FromHttpApiData PosixTS where
  parseUrlPiece x = PosixTS . posixSecondsToUTCTime . fromIntegral <$> parseUrlPiece @Int x

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
  deriving newtype (ToJSON, FromJSON, Hashable, FromHttpApiData)

$(deriveSafeCopy 0 'base ''ID)

inactivityTimeout :: NominalDiffTime
inactivityTimeout = 30

data Grid
  = Grid
  { _bb_uuid :: !ID
  , _bb_name :: !Text
  , _bb_mapUids :: ![(Text)]
  , _bb_size :: !Int
  , _bb_chatLog :: !(Seq ChatMessage)
  , _bb_authorUid :: !Text
  , _bb_players :: !(Map Text PosixTS)
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

newtype InactiveAfter = InactiveAfter UTCTime
  deriving (Eq, Show, Ord)

type BBIndexes = '[ID, CreatedAt, InactiveAfter]
type BBTable = IxSet BBIndexes Grid

instance IxSet.Indexable BBIndexes Grid where
  indices = ixList
    (ixFun $ \bb -> [_bb_uuid bb])
    (ixFun $ \bb -> [CreatedAt $ unPosixTS $ _bb_createdAt bb])
    (ixFun $ \bb -> [InactiveAfter $ fromMaybe (posixSecondsToUTCTime 0) $ fmap (unPosixTS . Data.Foldable1.maximum) $ nonEmpty $ Map.elems $ _bb_players bb])

data BBTableUpdate
  = BBTUAddBoard Grid
  | BBTUAddChatMessage ID ChatMessage
  | BBTUPingConnected ID Text
  | BBTUDisconnected ID Text

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

getActiveBoards :: UTCTime -> Int -> Query BBTable [Grid]
getActiveBoards now n = do
  asks $ Protolude.take n . toDescList (Proxy @CreatedAt) . (@> InactiveAfter now)

updateBBTable :: UTCTime -> BBTableUpdate -> Update BBTable ()
updateBBTable _   (BBTUAddBoard board) = do
  modify $ updateIx (_bb_uuid board) board
updateBBTable now (BBTUAddChatMessage gId msg) = do
  gets (getOne . getEQ gId)
    >>= \case
      Nothing -> pass
      Just board -> do
        modify $ updateIx gId ((addChatMessage board msg) {_bb_updatedAt = PosixTS now}) 
updateBBTable now (BBTUPingConnected gId userUid) = do
  gets (getOne . getEQ gId)
    >>= \case
      Nothing -> pass
      Just board -> do
        modify $ updateIx gId $
          board { _bb_players = Map.insert userUid (PosixTS $ addUTCTime inactivityTimeout now) $ Map.filter (> PosixTS now) $ _bb_players board
                -- , _bb_updatedAt = PosixTS now
                } 
updateBBTable now (BBTUDisconnected gId userUid) = do
  gets (getOne . getEQ gId)
    >>= \case
      Nothing -> pass
      Just board -> do
        modify $ updateIx gId $
          board { _bb_players = Map.filter (> PosixTS now) $ Map.delete userUid $ _bb_players board
                -- , _bb_updatedAt = PosixTS now
                }

$(makeAcidic ''BBTable ['getBoard, 'updateBBTable, 'getLastBoards, 'getBoardUpdatedAfter, 'getActiveBoards])
