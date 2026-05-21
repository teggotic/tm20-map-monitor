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

module MapMonitor.DB (
  TMMap (..),
  TMXMapType (..),
  TMMapPatch (..),
  TMMapPatchAction (..),
  tmmp_actions,
  TMInfo (..),
  TMXId (..),
  TMMapRecord (..),
  MapMonitorState (..),
  AddNewMaps (..),
  AddNewMaps' (..),
  GetMaps (..),
  GetMapMonitorState (..),
  HideMap (..),
  SetAtSetByPlugin (..),
  ReplaceMap (..),
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
  SetTrustedUsers (..),
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

import Control.Lens
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.Aeson.Types (ToJSON (toJSON))
import Data.IxSet.Typed hiding (fromList)
import qualified Data.IxSet.Typed as IxSet
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Exts (IsList (fromList))
import Protolude
import qualified RIO.Set as Set
import qualified RIO.Text as Text

data TMXMapType
  = MT_Race
  | MT_Royal
  | MT_Stunt
  | MT_Platform
  | MT_Puzzle
  | MT_Other Text
  deriving (Show, Eq, Ord, Generic)

$(deriveJSON defaultOptions ''TMXMapType)

$(deriveSafeCopy 0 'base ''TMXMapType)

data TMMapRecord
  = TMMapRecord
  { _tmmr_userId :: !Text
  , _tmmr_time :: !Int
  , _tmmr_timestamp :: !Int
  }
  deriving (Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmmr_")} ''TMMapRecord)
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

data TMMap_v9
  = TMMap_v9
  { v9_tmm_tmxId :: !TMXId
  , v9_tmm_uid :: !Text
  , v9_tmm_name :: !Text
  , v9_tmm_authorMedal :: !Int
  , v9_tmm_authorUid :: !((Maybe Text))
  , v9_tmm_tags :: ![Int]
  , v9_tmm_currentWR :: !((Maybe TMMapRecord))
  , v9_tmm_uploadedAt :: !((Maybe UTCTime))
  , v9_tmm_hiddenReason :: !((Maybe Text))
  , v9_tmm_atSetByPlugin :: !((Maybe Bool))
  , v9_tmm_nbPlayers :: !((Maybe Int))
  , v9_tmm_reportedBy :: !(Map Text (UTCTime, Text))
  , v9_tmm_mapType :: !(Maybe TMXMapType)
  , v9_tmm_mapVersions :: ![TMMap_v9]
  , v9_tmm_hiddenOnTmx :: !Bool
  , v9_tmm_beatenPingSent :: !Bool
  , v9_tmm_validationReplay :: !(Maybe (Maybe Text, UTCTime))
  }
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmm_")} ''TMMap_v9)

instance Migrate TMMap_v9 where
  type MigrateFrom TMMap_v9 = TMMap_v8
  migrate (TMMap_v8{..}) =
    TMMap_v9
      { v9_tmm_tmxId = v8_tmm_tmxId
      , v9_tmm_uid = v8_tmm_uid
      , v9_tmm_name = v8_tmm_name
      , v9_tmm_authorMedal = v8_tmm_authorMedal
      , v9_tmm_authorUid = v8_tmm_authorUid
      , v9_tmm_tags = v8_tmm_tags
      , v9_tmm_currentWR = v8_tmm_currentWR
      , v9_tmm_uploadedAt = v8_tmm_uploadedAt
      , v9_tmm_hiddenReason = v8_tmm_hiddenReason
      , v9_tmm_atSetByPlugin = v8_tmm_atSetByPlugin
      , v9_tmm_nbPlayers = v8_tmm_nbPlayers
      , v9_tmm_reportedBy = v8_tmm_reportedBy
      , v9_tmm_mapType = v8_tmm_mapType
      , v9_tmm_mapVersions = migrate <$> v8_tmm_mapVersions
      , v9_tmm_hiddenOnTmx = v8_tmm_hiddenOnTmx
      , v9_tmm_beatenPingSent = maybe False (\wr -> _tmmr_time wr < v8_tmm_authorMedal) v8_tmm_currentWR
      , v9_tmm_validationReplay = Nothing
      }

$(deriveSafeCopy 9 'extension ''TMMap_v9)

data TMInfo
  = TMCheatedAt
  | TMBrokenPhysics
  deriving (Show, Eq, Ord)

$(deriveJSON defaultOptions{constructorTagModifier = drop (Text.length "TM")} ''TMInfo)
$(deriveSafeCopy 0 'base ''TMInfo)

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
  , _tmm_info :: !(Set TMInfo)
  , _tmm_hasClones :: !(Maybe Bool)
  , _tmm_omittedFromPlugin :: !Bool
  , _tmm_fileSize :: !(Maybe Int)
  }
  deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = drop (Text.length "_tmm_")} ''TMMap)

instance Migrate TMMap where
  type MigrateFrom TMMap = TMMap_v9
  migrate (TMMap_v9{..}) =
    TMMap
      { _tmm_tmxId = v9_tmm_tmxId
      , _tmm_uid = v9_tmm_uid
      , _tmm_name = v9_tmm_name
      , _tmm_authorMedal = v9_tmm_authorMedal
      , _tmm_authorUid = v9_tmm_authorUid
      , _tmm_tags = v9_tmm_tags
      , _tmm_currentWR = v9_tmm_currentWR
      , _tmm_uploadedAt = v9_tmm_uploadedAt
      , _tmm_hiddenReason = v9_tmm_hiddenReason
      , _tmm_atSetByPlugin = v9_tmm_atSetByPlugin
      , _tmm_nbPlayers = v9_tmm_nbPlayers
      , _tmm_reportedBy = v9_tmm_reportedBy
      , _tmm_mapType = v9_tmm_mapType
      , _tmm_mapVersions = migrate <$> v9_tmm_mapVersions
      , _tmm_hiddenOnTmx = v9_tmm_hiddenOnTmx
      , _tmm_beatenPingSent = v9_tmm_beatenPingSent
      , _tmm_validationReplay = v9_tmm_validationReplay
      , _tmm_info = mempty
      , _tmm_hasClones = Nothing
      , _tmm_omittedFromPlugin = False
      , _tmm_fileSize = Nothing
      }

$(deriveSafeCopy 10 'extension ''TMMap)

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

newtype OmittedFromPlugin = OmittedFromPlugin Bool
  deriving (Show, Eq, Ord)

type TMMapIxs = '[TMXId, IsBeaten, TrackType, HiddenOnTmx, UploadedAt, HasNadeoInfo, WrTimestamp, OmittedFromPlugin]
type IxEntry = IxSet TMMapIxs TMMap

instance ToJSON (IxSet TMMapIxs TMMap) where
  toJSON = toJSON . IxSet.toAscList (Proxy @TMXId)

instance IxSet.Indexable TMMapIxs TMMap where
  indices =
    ixList
      (ixFun $ \tmMap -> [_tmm_tmxId tmMap])
      (ixFun $ \tmMap -> [bool Beaten Unbeaten $ isMapUnbeaten tmMap])
      (ixFun $ \tmMap -> [TrackType $ _tmm_mapType tmMap])
      (ixFun $ \tmMap -> [HiddenOnTmx $ _tmm_hiddenOnTmx tmMap])
      (ixFun $ \tmMap -> catMaybes [UploadedAt <$> _tmm_uploadedAt tmMap])
      (ixFun $ \tmMap -> [HasNadeoInfo $ isJust $ _tmm_authorUid tmMap])
      (ixFun $ \tmMap -> maybe [] (pure . WrTimestamp . posixSecondsToUTCTime . secondsToNominalDiffTime . fromIntegral . _tmmr_timestamp) $ _tmm_currentWR tmMap)
      (ixFun $ \tmMap -> [OmittedFromPlugin $ _tmm_omittedFromPlugin tmMap])

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

data TMMapPatch_v4
  = TMMapPatch_v4
  { v4_tmmp_tmxId :: !TMXId
  , v4_tmmp_uid :: !(Maybe Text)
  , v4_tmmp_name :: !(Maybe Text)
  , v4_tmmp_authorMedal :: !(Maybe Int)
  , v4_tmmp_authorUid :: !(Maybe (Maybe Text))
  , v4_tmmp_tags :: !(Maybe [Int])
  , v4_tmmp_currentWR :: !(Maybe (Maybe TMMapRecord))
  , v4_tmmp_uploadedAt :: !(Maybe (Maybe UTCTime))
  , v4_tmmp_hiddenReason :: !(Maybe (Maybe Text))
  , v4_tmmp_atSetByPlugin :: !(Maybe (Maybe Bool))
  , v4_tmmp_nbPlayers :: !(Maybe (Maybe Int))
  , v4_tmmp_reportedBy :: !(Maybe (Map Text (Maybe (UTCTime, Text))))
  , v4_tmmp_mapType :: !(Maybe (Maybe TMXMapType))
  , v4_tmmp_mapVersions :: !(Maybe TMMap)
  , v4_tmmp_hiddenOnTmx :: !(Maybe Bool)
  , v4_tmmp_beatenPingSent :: !(Maybe Bool)
  , v4_tmmp_validationReplay :: !(Maybe (Maybe (Maybe Text, UTCTime)))
  }
  deriving (Show)

instance Migrate TMMapPatch_v4 where
  type MigrateFrom TMMapPatch_v4 = TMMapPatch_v3
  migrate (TMMapPatch_v3{..}) =
    TMMapPatch_v4
      { v4_tmmp_tmxId = v3_tmmp_tmxId
      , v4_tmmp_uid = v3_tmmp_uid
      , v4_tmmp_name = v3_tmmp_name
      , v4_tmmp_authorMedal = v3_tmmp_authorMedal
      , v4_tmmp_authorUid = v3_tmmp_authorUid
      , v4_tmmp_tags = v3_tmmp_tags
      , v4_tmmp_currentWR = v3_tmmp_currentWR
      , v4_tmmp_uploadedAt = v3_tmmp_uploadedAt
      , v4_tmmp_hiddenReason = v3_tmmp_hiddenReason
      , v4_tmmp_atSetByPlugin = v3_tmmp_atSetByPlugin
      , v4_tmmp_nbPlayers = v3_tmmp_nbPlayers
      , v4_tmmp_reportedBy = v3_tmmp_reportedBy
      , v4_tmmp_mapType = v3_tmmp_mapType
      , v4_tmmp_mapVersions = v3_tmmp_mapVersions
      , v4_tmmp_hiddenOnTmx = v3_tmmp_hiddenOnTmx
      , v4_tmmp_beatenPingSent = Nothing
      , v4_tmmp_validationReplay = Nothing
      }

$(deriveSafeCopy 4 'extension ''TMMapPatch_v4)

data TMMapPatchAction
  = TMPUid Text
  | TMPName Text
  | TMPAuthorMedal Int
  | TMPAuthorUid (Maybe Text)
  | TMPTags [Int]
  | TMPCurrentWR (Maybe TMMapRecord)
  | TMPUploadedAt (Maybe UTCTime)
  | TMPHiddenReason (Maybe Text)
  | TMPAtSetByPlugin (Maybe Bool)
  | TMPNbPlayers (Maybe Int)
  | TMPReportedBy (Map Text (Maybe (UTCTime, Text)))
  | TMPMapType (Maybe TMXMapType)
  | TMPMapVersions TMMap
  | TMPHiddenOnTmx Bool
  | TMPBeatenPingSent Bool
  | TMPValidationReplay (Maybe (Maybe Text, UTCTime))
  | TMPAddInfo TMInfo
  | TMPRemoveInfo TMInfo
  | TMPClones (Maybe Bool)
  | TMPOmittedFromPlugin Bool
  | TMPFileSize Int
  deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''TMMapPatchAction)

data TMMapPatch
  = TMMapPatch
  { _tmmp_tmxId :: TMXId
  , _tmmp_actions :: [TMMapPatchAction]
  }
  deriving (Show, Eq)

$(makeLenses ''TMMapPatch)

instance Migrate TMMapPatch where
  type MigrateFrom TMMapPatch = TMMapPatch_v4
  migrate (TMMapPatch_v4{..}) =
    TMMapPatch
      { _tmmp_tmxId = v4_tmmp_tmxId
      , _tmmp_actions =
          catMaybes $
            [ TMPUid <$> v4_tmmp_uid
            , TMPName <$> v4_tmmp_name
            , TMPAuthorMedal <$> v4_tmmp_authorMedal
            , TMPAuthorUid <$> v4_tmmp_authorUid
            , TMPTags <$> v4_tmmp_tags
            , TMPCurrentWR <$> v4_tmmp_currentWR
            , TMPUploadedAt <$> v4_tmmp_uploadedAt
            , TMPHiddenReason <$> v4_tmmp_hiddenReason
            , TMPAtSetByPlugin <$> v4_tmmp_atSetByPlugin
            , TMPNbPlayers <$> v4_tmmp_nbPlayers
            , TMPReportedBy <$> v4_tmmp_reportedBy
            , TMPMapType <$> v4_tmmp_mapType
            , TMPMapVersions <$> v4_tmmp_mapVersions
            , TMPHiddenOnTmx <$> v4_tmmp_hiddenOnTmx
            , TMPBeatenPingSent <$> v4_tmmp_beatenPingSent
            , TMPValidationReplay <$> v4_tmmp_validationReplay
            ]
      }

$(deriveSafeCopy 5 'extension ''TMMapPatch)

defPatch :: TMXId -> TMMapPatch
defPatch idx = TMMapPatch idx []

patchIsEmpty :: TMMapPatch -> Bool
patchIsEmpty = Protolude.null . _tmmp_actions

applyMapChanges :: (Ord k) => Map k a -> Map k (Maybe a) -> Map k a
applyMapChanges mp updates = foldl' (\acc (k, val) -> Map.alter (const val) k acc) mp (Map.assocs updates)

applyPatch :: TMMapPatch -> TMMap -> TMMap
applyPatch patch tmMap =
  foldl' go tmMap (_tmmp_actions patch)
 where
  go acc (TMPUid uid) = acc{_tmm_uid = uid}
  go acc (TMPName name) = acc{_tmm_name = name}
  go acc (TMPAuthorMedal medal) = acc{_tmm_authorMedal = medal}
  go acc (TMPAuthorUid authorUid) = acc{_tmm_authorUid = authorUid}
  go acc (TMPTags tags) = acc{_tmm_tags = tags}
  go acc (TMPCurrentWR wr) = acc{_tmm_currentWR = wr}
  go acc (TMPUploadedAt uploadedAt) = acc{_tmm_uploadedAt = uploadedAt}
  go acc (TMPHiddenReason hiddenReason) = acc{_tmm_hiddenReason = hiddenReason}
  go acc (TMPAtSetByPlugin atSetByPlugin) = acc{_tmm_atSetByPlugin = atSetByPlugin}
  go acc (TMPNbPlayers nbPlayers) = acc{_tmm_nbPlayers = nbPlayers}
  go acc (TMPReportedBy reportedBy) = acc{_tmm_reportedBy = applyMapChanges (_tmm_reportedBy acc) reportedBy}
  go acc (TMPMapType mapType) = acc{_tmm_mapType = mapType}
  go acc (TMPMapVersions mapVersion) = acc{_tmm_mapVersions = mapVersion : _tmm_mapVersions acc}
  go acc (TMPHiddenOnTmx hiddenOnTmx) = acc{_tmm_hiddenOnTmx = hiddenOnTmx}
  go acc (TMPBeatenPingSent beatenPingSent) = acc{_tmm_beatenPingSent = beatenPingSent}
  go acc (TMPValidationReplay validationReplay) = acc{_tmm_validationReplay = validationReplay}
  go acc (TMPAddInfo info) = acc{_tmm_info = Set.insert info (_tmm_info acc)}
  go acc (TMPRemoveInfo info) = acc{_tmm_info = Set.delete info (_tmm_info acc)}
  go acc (TMPClones clones) = acc{_tmm_hasClones = clones}
  go acc (TMPOmittedFromPlugin omittedFromPlugin) = acc{_tmm_omittedFromPlugin = omittedFromPlugin}
  go acc (TMPFileSize fileSize) = acc{_tmm_fileSize = Just fileSize}

data MapMonitorState_v3
  = MapMonitorState_v3
  { v3_mms_maps :: IxEntry
  }
  deriving (Show)

$(deriveSafeCopy 3 'base ''MapMonitorState_v3)

data MapMonitorState
  = MapMonitorState
  { _mms_maps :: !IxEntry
  , _mms_trustedUsers :: !(Set Text)
  }
  deriving (Show)

instance Migrate MapMonitorState where
  type MigrateFrom MapMonitorState = MapMonitorState_v3
  migrate (MapMonitorState_v3{..}) =
    MapMonitorState v3_mms_maps mempty

$(deriveSafeCopy 4 'extension ''MapMonitorState)

$(makeLenses ''MapMonitorState)

isMapNewVersion :: TMMap -> TMMap -> Bool
isMapNewVersion mp dbmap =
  or
    [ _tmm_uid dbmap /= _tmm_uid mp
    , _tmm_authorMedal dbmap /= _tmm_authorMedal mp
    , _tmm_mapType dbmap /= _tmm_mapType mp
    , _tmm_name dbmap /= _tmm_name mp
    ]

patchDB :: [TMMapPatch] -> IxEntry -> IxEntry
patchDB [] db = db
patchDB patches db =
  foldl' go db patches
 where
  go acc p =
    case getOne (acc @= _tmmp_tmxId p) of
      Nothing -> acc
      Just m ->
        let updated = (applyPatch p m)
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
      if _tmm_uid m /= _tmm_uid mp || _tmm_authorMedal m /= _tmm_authorMedal mp
        then do
          let newMap = mp{_tmm_mapVersions = m : _tmm_mapVersions m}
          mms_maps %= IxSet.updateIx (_tmm_tmxId newMap) newMap
          return $ Just newMap
        else
          if isMapNewVersion mp m
            then do
              let newMap =
                    m
                      { _tmm_mapType = _tmm_mapType mp
                      , _tmm_name = _tmm_name mp
                      }
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
  asks $ filter (\x -> (_tmm_hasClones x /= Just True)) . IxSet.toList . (@= (HiddenOnTmx False)) . (@= HasNadeoInfo True) . (@= (TrackType $ Just MT_Race)) . (@= Unbeaten) . _mms_maps

getMapMonitorState :: Query MapMonitorState MapMonitorState
getMapMonitorState = ask

replaceMap :: TMMap -> Update MapMonitorState ()
replaceMap tmMap = do
  mms_maps %= IxSet.updateIx (_tmm_tmxId tmMap) tmMap

hideMap :: TMXId -> Text -> Update MapMonitorState ()
hideMap tmxId reason = do
  updateMaps' [TMMapPatch tmxId [TMPHiddenReason (Just reason)]]

setAtSetByPlugin :: TMXId -> Maybe Bool -> Update MapMonitorState ()
setAtSetByPlugin tmxId atSetByPlugin = do
  updateMaps' [TMMapPatch tmxId [TMPAtSetByPlugin atSetByPlugin]]

setTrustedUsers :: Set Text -> Update MapMonitorState ()
setTrustedUsers us = do
  mms_trustedUsers .= us

$(makeAcidic ''MapMonitorState ['updateMaps', 'addNewMaps, 'addNewMaps', 'getMaps, 'getMapMonitorState, 'hideMap, 'setAtSetByPlugin, 'getMapsByIds, 'removeMap, 'getMapById, 'isKnownId, 'getAllKnownIds, 'tryUpdateMapVersion, 'setTrustedUsers, 'replaceMap])

updateMaps :: (MonadIO m) => AcidState MapMonitorState -> [TMMapPatch] -> m [TMMap]
updateMaps _ [] = return []
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
        TMMapPatch tmxId [TMPReportedBy $ fromList [(userId, Just (now, message))]]
