{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MapMonitor.DB (
  TMMap (..),
  TMXMapType (..),
  TMMapPatch (..),
  TMXId (..),
  TMMapRecord (..),
  MapMonitorState (..),
  AddNewMaps (..),
  AddNewMaps' (..),
  GetMaps (..),
  GetMapMonitorState (..),
  HideMap (..),
  SetAtSetByPlugin (..),
  RemoveMap (..),
  GetMapById (..),
  GetMapsByIds (..),
  IsKnownId (..),
  GetAllKnownIds (..),
  UploadedAt (..),
  HiddenOnTmx (..),
  TrackType (..),
  IsBeaten (..),
  HasNadeoInfo (..),
  WrTimestamp (..),
  TryUpdateMapVersion (..),
  TMMapIxs,
  IxEntry,
  reportMap,
  applyPatch,
  defPatch,
  patchIsEmpty,
  updateMaps,
  isMapUnbeaten,
  insertMissingMaps,
  isMapNewVersion,
)
where

import Data.Acid
import Data.Acid.Advanced
import Data.Aeson (FromJSON, ToJSON)
import Data.IxSet.Typed hiding (fromList)
import qualified Data.IxSet.Typed as IxSet
import qualified Data.Map as Map
import Control.Lens
import Data.SafeCopy
import Data.Time
import GHC.Exts (IsList (fromList))
import Protolude
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Aeson.TH
import qualified RIO.Text as Text
import Data.Aeson.Types (ToJSON(toJSON))

data TMXMapType
  = MT_Race
  | MT_Royal
  | MT_Stunt
  | MT_Platform
  | MT_Puzzle
  | MT_Other Text
  deriving (Show, Eq, Ord, Generic)

$(deriveToJSON defaultOptions ''TMXMapType)

$(deriveSafeCopy 0 'base ''TMXMapType)

data TMMapRecord
  = TMMapRecord
  { _tmmr_userId :: !Text
  , _tmmr_time :: !Int
  , _tmmr_timestamp :: !Int
  }
  deriving (Show, Eq)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmmr_")} ''TMMapRecord)
$(deriveSafeCopy 0 'base ''TMMapRecord)

newtype TMXId
  = TMXId {unTMXId :: Int}
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, Num)

$(deriveSafeCopy 0 'base ''TMXId)

data TMMap_v8
  = TMMap_v8
  { v8_tmm_tmxId :: !TMXId
  , v8_tmm_uid :: !Text
  , v8_tmm_name :: !Text
  , v8_tmm_authorMedal :: !Int
  , v8_tmm_authorUid :: !((Maybe Text))
  , v8_tmm_tags :: ![Int]
  , v8_tmm_currentWR :: !((Maybe TMMapRecord))
  , v8_tmm_uploadedAt :: !((Maybe UTCTime))
  , v8_tmm_hiddenReason :: !((Maybe Text))
  , v8_tmm_atSetByPlugin :: !((Maybe Bool))
  , v8_tmm_nbPlayers :: !((Maybe Int))
  , v8_tmm_reportedBy :: !(Map Text (UTCTime, Text))
  , v8_tmm_mapType :: !(Maybe TMXMapType)
  , v8_tmm_mapVersions :: ![TMMap_v8]
  , v8_tmm_hiddenOnTmx :: !Bool
  }
  deriving (Generic, Show, Eq)

$(deriveSafeCopy 8 'base ''TMMap_v8)

data TMMap
  = TMMap
  { _tmm_tmxId :: !TMXId
  , _tmm_uid :: !Text
  , _tmm_name :: !Text
  , _tmm_authorMedal :: !Int
  , _tmm_authorUid :: !((Maybe Text))
  , _tmm_tags :: ![Int]
  , _tmm_currentWR :: !((Maybe TMMapRecord))
  , _tmm_uploadedAt :: !((Maybe UTCTime))
  , _tmm_hiddenReason :: !((Maybe Text))
  , _tmm_atSetByPlugin :: !((Maybe Bool))
  , _tmm_nbPlayers :: !((Maybe Int))
  , _tmm_reportedBy :: !(Map Text (UTCTime, Text))
  , _tmm_mapType :: !(Maybe TMXMapType)
  , _tmm_mapVersions :: ![TMMap]
  , _tmm_hiddenOnTmx :: !Bool
  , _tmm_beatenPingSent :: !Bool
  , _tmm_validationReplay :: !(Maybe (Maybe Text, UTCTime))
  }
  deriving (Generic, Show, Eq)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmm_")} ''TMMap)

instance Migrate TMMap where
  type MigrateFrom TMMap = TMMap_v8
  migrate (TMMap_v8 {..}) =
    TMMap
      { _tmm_tmxId = v8_tmm_tmxId
      , _tmm_uid = v8_tmm_uid
      , _tmm_name = v8_tmm_name
      , _tmm_authorMedal = v8_tmm_authorMedal
      , _tmm_authorUid = v8_tmm_authorUid
      , _tmm_tags = v8_tmm_tags
      , _tmm_currentWR = v8_tmm_currentWR
      , _tmm_uploadedAt = v8_tmm_uploadedAt
      , _tmm_hiddenReason = v8_tmm_hiddenReason
      , _tmm_atSetByPlugin = v8_tmm_atSetByPlugin
      , _tmm_nbPlayers = v8_tmm_nbPlayers
      , _tmm_reportedBy = v8_tmm_reportedBy
      , _tmm_mapType = v8_tmm_mapType
      , _tmm_mapVersions = migrate <$> v8_tmm_mapVersions
      , _tmm_hiddenOnTmx = v8_tmm_hiddenOnTmx
      , _tmm_beatenPingSent = maybe False (\wr -> _tmmr_time wr < v8_tmm_authorMedal) v8_tmm_currentWR
      , _tmm_validationReplay = Nothing
      }

$(deriveSafeCopy 9 'extension ''TMMap)

isMapUnbeaten :: TMMap -> Bool
isMapUnbeaten tmMap =
  maybe True (\wr -> _tmmr_time wr > _tmm_authorMedal tmMap) (_tmm_currentWR tmMap)

data IsBeaten
  = Beaten
  | Unbeaten
  deriving (Show, Eq, Ord)

newtype TrackType = TrackType (Maybe TMXMapType)
  deriving (Show, Eq, Ord)

instance Ord TMMap where
  compare = comparing _tmm_tmxId

newtype HiddenOnTmx = HiddenOnTmx Bool
  deriving (Show, Eq, Ord)

newtype UploadedAt = UploadedAt UTCTime
  deriving (Show, Eq, Ord)

newtype HasNadeoInfo = HasNadeoInfo Bool
  deriving (Show, Eq, Ord)

newtype WrTimestamp = WrTimestamp UTCTime
  deriving (Show, Eq, Ord)

type TMMapIxs = '[TMXId, IsBeaten, TrackType, HiddenOnTmx, UploadedAt, HasNadeoInfo, WrTimestamp]
type IxEntry  = IxSet TMMapIxs TMMap

instance ToJSON (IxSet TMMapIxs TMMap) where
  toJSON = toJSON . IxSet.toAscList (Proxy @TMXId)

instance IxSet.Indexable TMMapIxs TMMap where
  indices = ixList
    (ixFun $ \tmMap -> [_tmm_tmxId tmMap])
    (ixFun $ \tmMap -> [bool Beaten Unbeaten $ isMapUnbeaten tmMap])
    (ixFun $ \tmMap -> [TrackType $ _tmm_mapType tmMap])
    (ixFun $ \tmMap -> [HiddenOnTmx $ _tmm_hiddenOnTmx tmMap])
    (ixFun $ \tmMap -> catMaybes [UploadedAt <$> _tmm_uploadedAt tmMap])
    (ixFun $ \tmMap -> [HasNadeoInfo $ isJust $ _tmm_authorUid tmMap])
    (ixFun $ \tmMap -> maybe [] (pure . WrTimestamp . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral .  _tmmr_timestamp) $ _tmm_currentWR tmMap)


data TMMapPatch_v3
  = TMMapPatch_v3
  { v3_tmmp_tmxId :: !TMXId
  , v3_tmmp_uid :: !(Maybe Text)
  , v3_tmmp_name :: !(Maybe Text)
  , v3_tmmp_authorMedal :: !(Maybe Int)
  , v3_tmmp_authorUid :: !(Maybe (Maybe Text))
  , v3_tmmp_tags :: !(Maybe [Int])
  , v3_tmmp_currentWR :: !(Maybe (Maybe TMMapRecord))
  , v3_tmmp_uploadedAt :: !(Maybe (Maybe UTCTime))
  , v3_tmmp_hiddenReason :: !(Maybe (Maybe Text))
  , v3_tmmp_atSetByPlugin :: !(Maybe (Maybe Bool))
  , v3_tmmp_nbPlayers :: !(Maybe (Maybe Int))
  , v3_tmmp_reportedBy :: !(Maybe (Map Text (Maybe (UTCTime, Text))))
  , v3_tmmp_mapType :: !(Maybe (Maybe TMXMapType))
  , v3_tmmp_mapVersions :: !(Maybe TMMap)
  , v3_tmmp_hiddenOnTmx :: !(Maybe Bool)
  }
  deriving (Show)

$(deriveSafeCopy 3 'base ''TMMapPatch_v3)

data TMMapPatch
  = TMMapPatch
  { _tmmp_tmxId :: !TMXId
  , _tmmp_uid :: !(Maybe Text)
  , _tmmp_name :: !(Maybe Text)
  , _tmmp_authorMedal :: !(Maybe Int)
  , _tmmp_authorUid :: !(Maybe (Maybe Text))
  , _tmmp_tags :: !(Maybe [Int])
  , _tmmp_currentWR :: !(Maybe (Maybe TMMapRecord))
  , _tmmp_uploadedAt :: !(Maybe (Maybe UTCTime))
  , _tmmp_hiddenReason :: !(Maybe (Maybe Text))
  , _tmmp_atSetByPlugin :: !(Maybe (Maybe Bool))
  , _tmmp_nbPlayers :: !(Maybe (Maybe Int))
  , _tmmp_reportedBy :: !(Maybe (Map Text (Maybe (UTCTime, Text))))
  , _tmmp_mapType :: !(Maybe (Maybe TMXMapType))
  , _tmmp_mapVersions :: !(Maybe TMMap)
  , _tmmp_hiddenOnTmx :: !(Maybe Bool)
  , _tmmp_beatenPingSent :: !(Maybe Bool)
  , _tmmp_validationReplay :: !(Maybe (Maybe (Maybe Text, UTCTime)))
  }
  deriving (Show)

instance Migrate TMMapPatch where
  type MigrateFrom TMMapPatch = TMMapPatch_v3
  migrate (TMMapPatch_v3 {..}) =
    TMMapPatch
      { _tmmp_tmxId = v3_tmmp_tmxId
      , _tmmp_uid = v3_tmmp_uid
      , _tmmp_name = v3_tmmp_name
      , _tmmp_authorMedal = v3_tmmp_authorMedal
      , _tmmp_authorUid = v3_tmmp_authorUid
      , _tmmp_tags = v3_tmmp_tags
      , _tmmp_currentWR = v3_tmmp_currentWR
      , _tmmp_uploadedAt = v3_tmmp_uploadedAt
      , _tmmp_hiddenReason = v3_tmmp_hiddenReason
      , _tmmp_atSetByPlugin = v3_tmmp_atSetByPlugin
      , _tmmp_nbPlayers = v3_tmmp_nbPlayers
      , _tmmp_reportedBy = v3_tmmp_reportedBy
      , _tmmp_mapType = v3_tmmp_mapType
      , _tmmp_mapVersions = v3_tmmp_mapVersions
      , _tmmp_hiddenOnTmx = v3_tmmp_hiddenOnTmx
      , _tmmp_beatenPingSent = Nothing
      , _tmmp_validationReplay = Nothing
      }

$(deriveSafeCopy 4 'extension ''TMMapPatch)

defPatch :: TMXId -> TMMapPatch
defPatch idx = TMMapPatch idx Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

patchIsEmpty :: TMMapPatch -> Bool
patchIsEmpty patch =
  and
    [ isNothing $ _tmmp_uid patch
    , isNothing $ _tmmp_name patch
    , isNothing $ _tmmp_authorMedal patch
    , isNothing $ _tmmp_authorUid patch
    , isNothing $ _tmmp_tags patch
    , isNothing $ _tmmp_currentWR patch
    , isNothing $ _tmmp_uploadedAt patch
    , isNothing $ _tmmp_hiddenReason patch
    , isNothing $ _tmmp_atSetByPlugin patch
    , isNothing $ _tmmp_nbPlayers patch
    , maybe False Protolude.null $ _tmmp_reportedBy patch
    , isNothing $ _tmmp_mapVersions patch
    , isNothing $ _tmmp_hiddenOnTmx patch
    , isNothing $ _tmmp_beatenPingSent patch
    , isNothing $ _tmmp_validationReplay patch
    ]

applyPatch :: TMMapPatch -> TMMap -> TMMap
applyPatch patch tmMap =
  TMMap
    { _tmm_tmxId = _tmmp_tmxId patch
    , _tmm_uid = fromMaybe (_tmm_uid tmMap) (_tmmp_uid patch)
    , _tmm_name = fromMaybe (_tmm_name tmMap) (_tmmp_name patch)
    , _tmm_authorMedal = fromMaybe (_tmm_authorMedal tmMap) (_tmmp_authorMedal patch)
    , _tmm_authorUid = fromMaybe (_tmm_authorUid tmMap) (_tmmp_authorUid patch)
    , _tmm_tags = fromMaybe (_tmm_tags tmMap) (_tmmp_tags patch)
    , _tmm_currentWR = fromMaybe (_tmm_currentWR tmMap) (_tmmp_currentWR patch)
    , _tmm_uploadedAt = fromMaybe (_tmm_uploadedAt tmMap) (_tmmp_uploadedAt patch)
    , _tmm_hiddenReason = fromMaybe (_tmm_hiddenReason tmMap) (_tmmp_hiddenReason patch)
    , _tmm_atSetByPlugin = fromMaybe (_tmm_atSetByPlugin tmMap) (_tmmp_atSetByPlugin patch)
    , _tmm_nbPlayers = fromMaybe (_tmm_nbPlayers tmMap) (_tmmp_nbPlayers patch)
    , _tmm_reportedBy = case _tmmp_reportedBy patch of
        Nothing -> _tmm_reportedBy tmMap
        Just updates -> foldl' (\acc (k, val) -> Map.alter (const val) k acc) (_tmm_reportedBy tmMap) (Map.assocs updates)
    , _tmm_mapType = fromMaybe (_tmm_mapType tmMap) (_tmmp_mapType patch)
    , _tmm_mapVersions = fromMaybe (_tmm_mapVersions tmMap) ((:_tmm_mapVersions tmMap) <$> _tmmp_mapVersions patch)
    , _tmm_hiddenOnTmx = fromMaybe (_tmm_hiddenOnTmx tmMap) (_tmmp_hiddenOnTmx patch)
    , _tmm_beatenPingSent = fromMaybe (_tmm_beatenPingSent tmMap) (_tmmp_beatenPingSent patch)
    , _tmm_validationReplay = fromMaybe (_tmm_validationReplay tmMap) (_tmmp_validationReplay patch)
    }

data MapMonitorState
  = MapMonitorState
  { _mms_maps :: IxEntry
  }
  deriving (Show)

$(deriveToJSON defaultOptions{fieldLabelModifier = drop (Text.length "_mms_")} ''MapMonitorState)

$(makeLenses ''MapMonitorState)

$(deriveSafeCopy 3 'base ''MapMonitorState)

isMapNewVersion :: TMMap -> TMMap -> Bool
isMapNewVersion mp dbmap = or
  [ _tmm_uid dbmap /= _tmm_uid mp
  , _tmm_authorMedal dbmap /= _tmm_authorMedal mp
  , _tmm_mapType dbmap /= _tmm_mapType mp
  ]

patchDB :: [TMMapPatch] -> IxEntry -> IxEntry
patchDB [] db = db
patchDB patches db =
  foldl' go db patches
  where
    go acc p =
      case getOne (acc @= _tmmp_tmxId p) of
        Nothing -> acc
        Just m -> let updated = (applyPatch p m)
         in if updated == m
               then acc
               else IxSet.updateIx (_tmmp_tmxId p) updated acc

insertMissingMaps :: [TMMap] -> IxEntry -> IxEntry
insertMissingMaps [] db = db
insertMissingMaps maps db =
  foldl' go db maps
  where
    go acc m =
      if IxSet.null (acc @= _tmm_tmxId m)
        then IxSet.insert m acc
        else acc

addNewMaps :: [(TMXId, TMMap)] -> Update MapMonitorState ()
addNewMaps [] = pass
addNewMaps maps = do
  mms_maps %= insertMissingMaps (snd <$> maps)

addNewMaps' :: [TMMap] -> Update MapMonitorState ()
addNewMaps' [] = pass
addNewMaps' maps = do
  mms_maps %= insertMissingMaps maps

updateMaps' :: [TMMapPatch] -> Update MapMonitorState ()
updateMaps' patches = do
  mms_maps %= patchDB patches

removeMap :: TMXId -> Update MapMonitorState ()
removeMap tmxId = do
  mms_maps %= IxSet.deleteIx tmxId

getAllKnownIds :: Query MapMonitorState (Set Int)
getAllKnownIds = do
  asks $ fromList . fmap (unTMXId . _tmm_tmxId) . IxSet.toList . _mms_maps

isKnownId :: TMXId -> Query MapMonitorState Bool
isKnownId tmxId = do
  asks $ isJust . getOne . (@= tmxId) . _mms_maps

getMapById :: TMXId -> Query MapMonitorState (Maybe TMMap)
getMapById tmxId = do
  asks $ getOne . (@= tmxId) . _mms_maps

tryUpdateMapVersion :: TMMap -> Update MapMonitorState (Maybe TMMap)
tryUpdateMapVersion mp = do
  db <- gets _mms_maps
  case getOne (db @= _tmm_tmxId mp) of
    Nothing -> return Nothing
    Just m ->
      if isMapNewVersion mp m
        then do
          let newMap = mp { _tmm_mapVersions = m : _tmm_mapVersions m }
          mms_maps %= IxSet.updateIx (_tmm_tmxId newMap) newMap
          return $ Just newMap
        else
          return Nothing

getMapsByIds :: [TMXId] -> Query MapMonitorState [TMMap]
getMapsByIds [] = return []
getMapsByIds ids = do
  asks $ IxSet.toList . (@+ ids) . _mms_maps

getMaps :: Query MapMonitorState [TMMap]
getMaps = do
  asks $ IxSet.toList . (@= (HiddenOnTmx False)) . (@= HasNadeoInfo True) . (@= (TrackType $ Just MT_Race)) . (@= Unbeaten) . _mms_maps

getMapMonitorState :: Query MapMonitorState MapMonitorState
getMapMonitorState = ask

hideMap :: TMXId -> Text -> Update MapMonitorState ()
hideMap tmxId reason = do
  updateMaps' [(defPatch tmxId) { _tmmp_hiddenReason = Just $ Just reason }]

setAtSetByPlugin :: TMXId -> Maybe Bool -> Update MapMonitorState ()
setAtSetByPlugin tmxId atSetByPlugin = do
  updateMaps' [(defPatch tmxId) { _tmmp_atSetByPlugin = Just $ atSetByPlugin }]

$(makeAcidic ''MapMonitorState ['updateMaps', 'addNewMaps, 'addNewMaps', 'getMaps, 'getMapMonitorState, 'hideMap, 'setAtSetByPlugin, 'getMapsByIds, 'removeMap, 'getMapById, 'isKnownId, 'getAllKnownIds, 'tryUpdateMapVersion])

updateMaps :: (MonadIO m) => AcidState MapMonitorState -> [TMMapPatch] -> m [TMMap]
updateMaps _    [] = return []
updateMaps acid patches = do
  let maps = filter (not . patchIsEmpty) patches
  if Protolude.null maps
    then pass
    else update' acid $ UpdateMaps' maps
  query' acid $ GetMapsByIds (_tmmp_tmxId <$> patches)

reportMap :: (MonadIO m) => AcidState MapMonitorState -> TMXId -> (Text, UTCTime, Text) -> m ()
reportMap acid tmxId (userId, now, message) = do
  update' acid $
    UpdateMaps' $
      pure $
        (defPatch tmxId){_tmmp_reportedBy = Just $ fromList [(userId, Just (now, message))]}
