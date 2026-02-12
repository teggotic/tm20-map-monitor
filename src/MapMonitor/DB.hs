{-# LANGUAGE DeriveGeneric #-}
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
  GetMaps (..),
  GetBeatenMaps (..),
  GetMapMonitorState (..),
  HideMap (..),
  SetAtSetByPlugin (..),
  RemoveMap (..),
  IsHidden (..),
  GetMapById (..),
  IsKnownId (..),
  GetAllKnownIds (..),
  TMMapIxs,
  IxEntry,
  reportMap,
  applyPatch,
  defPatch,
  patchIsEmpty,
  updateMaps,
  isMapUnbeaten,
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

data TMXMapType
  = MT_Race
  | MT_Royal
  | MT_Stunt
  | MT_Platform
  | MT_Puzzle
  | MT_Other Text
  deriving (Show, Eq, Ord, Generic)

$(deriveSafeCopy 0 'base ''TMXMapType)

data TMMapRecord
  = TMMapRecord
  { _tmmr_userId :: !Text
  , _tmmr_time :: !Int
  , _tmmr_timestamp :: !Int
  }
  deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''TMMapRecord)

newtype TMXId
  = TMXId {unTMXId :: Int}
  deriving (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

$(deriveSafeCopy 0 'base ''TMXId)

data TMMap_v4
  = TMMap_v4
  { v4_tmm_tmxId :: !Int
  , v4_tmm_uid :: !Text
  , v4_tmm_name :: !Text
  , v4_tmm_authorMedal :: !Int
  , v4_tmm_authorUid :: !(Maybe Text)
  , v4_tmm_tags :: ![Int]
  , v4_tmm_currentWR :: !(Maybe TMMapRecord)
  , v4_tmm_uploadedAt :: !(Maybe UTCTime)
  , v4_tmm_hiddenReason :: !(Maybe Text)
  , v4_tmm_atSetByPlugin :: !(Maybe Bool)
  , v4_tmm_nbPlayers :: !(Maybe Int)
  }
  deriving (Show)

$(deriveSafeCopy 4 'base ''TMMap_v4)

data TMMap_v5
  = TMMap_v5
  { v5_tmm_tmxId :: !TMXId
  , v5_tmm_uid :: !Text
  , v5_tmm_name :: !Text
  , v5_tmm_authorMedal :: !Int
  , v5_tmm_authorUid :: !((Maybe Text))
  , v5_tmm_tags :: ![Int]
  , v5_tmm_currentWR :: !((Maybe TMMapRecord))
  , v5_tmm_uploadedAt :: !((Maybe UTCTime))
  , v5_tmm_hiddenReason :: !((Maybe Text))
  , v5_tmm_atSetByPlugin :: !((Maybe Bool))
  , v5_tmm_nbPlayers :: !((Maybe Int))
  }
  deriving (Generic, Show)

instance Migrate TMMap_v5 where
  type MigrateFrom TMMap_v5 = TMMap_v4
  migrate (TMMap_v4 tmxId uid name authorMedal authorUid tags currentWR uploadedAt hiddenReason atSetByPlugin nbPlayers) =
    TMMap_v5
      { v5_tmm_tmxId = TMXId tmxId
      , v5_tmm_uid = uid
      , v5_tmm_name = name
      , v5_tmm_authorMedal = authorMedal
      , v5_tmm_authorUid = authorUid
      , v5_tmm_tags = tags
      , v5_tmm_currentWR = currentWR
      , v5_tmm_uploadedAt = uploadedAt
      , v5_tmm_hiddenReason = hiddenReason
      , v5_tmm_atSetByPlugin = atSetByPlugin
      , v5_tmm_nbPlayers = nbPlayers
      }

$(deriveSafeCopy 5 'extension ''TMMap_v5)

data TMMap_v6
  = TMMap_v6
  { v6_tmm_tmxId :: !TMXId
  , v6_tmm_uid :: !Text
  , v6_tmm_name :: !Text
  , v6_tmm_authorMedal :: !Int
  , v6_tmm_authorUid :: !((Maybe Text))
  , v6_tmm_tags :: ![Int]
  , v6_tmm_currentWR :: !((Maybe TMMapRecord))
  , v6_tmm_uploadedAt :: !((Maybe UTCTime))
  , v6_tmm_hiddenReason :: !((Maybe Text))
  , v6_tmm_atSetByPlugin :: !((Maybe Bool))
  , v6_tmm_nbPlayers :: !((Maybe Int))
  , v6_tmm_reportedBy :: !(Map Text (UTCTime, Text))
  }
  deriving (Generic, Show)

instance Migrate TMMap_v6 where
  type MigrateFrom TMMap_v6 = TMMap_v5
  migrate (TMMap_v5 tmxId uid name authorMedal authorUid tags currentWR uploadedAt hiddenReason atSetByPlugin nbPlayers) =
    TMMap_v6
      { v6_tmm_tmxId = tmxId
      , v6_tmm_uid = uid
      , v6_tmm_name = name
      , v6_tmm_authorMedal = authorMedal
      , v6_tmm_authorUid = authorUid
      , v6_tmm_tags = tags
      , v6_tmm_currentWR = currentWR
      , v6_tmm_uploadedAt = uploadedAt
      , v6_tmm_hiddenReason = hiddenReason
      , v6_tmm_atSetByPlugin = atSetByPlugin
      , v6_tmm_nbPlayers = nbPlayers
      , v6_tmm_reportedBy = mempty
      }

$(deriveSafeCopy 6 'extension ''TMMap_v6)

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
  }
  deriving (Generic, Show, Eq)

instance Migrate TMMap where
  type MigrateFrom TMMap = TMMap_v6
  migrate (TMMap_v6 {..}) =
    TMMap
      { _tmm_tmxId = v6_tmm_tmxId
      , _tmm_uid = v6_tmm_uid
      , _tmm_name = v6_tmm_name
      , _tmm_authorMedal = v6_tmm_authorMedal
      , _tmm_authorUid = v6_tmm_authorUid
      , _tmm_tags = v6_tmm_tags
      , _tmm_currentWR = v6_tmm_currentWR
      , _tmm_uploadedAt = v6_tmm_uploadedAt
      , _tmm_hiddenReason = v6_tmm_hiddenReason
      , _tmm_atSetByPlugin = v6_tmm_atSetByPlugin
      , _tmm_nbPlayers = v6_tmm_nbPlayers
      , _tmm_reportedBy = v6_tmm_reportedBy
      , _tmm_mapType = Nothing
      }

$(deriveSafeCopy 7 'extension ''TMMap)

isMapUnbeaten :: TMMap -> Bool
isMapUnbeaten tmMap =
  maybe True (\wr -> _tmmr_time wr > _tmm_authorMedal tmMap) (_tmm_currentWR tmMap)

newtype IsHidden = IsHidden Bool
  deriving (Show, Eq, Ord)

data IsBeaten
  = Beaten
  | Unbeaten
  deriving (Show, Eq, Ord)

instance Ord TMMap where
  compare = comparing _tmm_tmxId

type TMMapIxs = '[TMXId, IsHidden, IsBeaten]
type IxEntry  = IxSet TMMapIxs TMMap

instance IxSet.Indexable TMMapIxs TMMap where
  indices = ixList
    (ixFun $ \tmMap -> [_tmm_tmxId tmMap])
    (ixFun $ \tmMap -> [IsHidden $ isJust $ _tmm_hiddenReason tmMap])
    (ixFun $ \tmMap -> [bool Beaten Unbeaten $ isMapUnbeaten tmMap])

data TMMapPatch_v0
  = TMMapPatch_v0
  { v0_tmmp_tmxId :: !TMXId
  , v0_tmmp_uid :: !(Maybe Text)
  , v0_tmmp_name :: !(Maybe Text)
  , v0_tmmp_authorMedal :: !(Maybe Int)
  , v0_tmmp_authorUid :: !(Maybe (Maybe Text))
  , v0_tmmp_tags :: !(Maybe [Int])
  , v0_tmmp_currentWR :: !(Maybe (Maybe TMMapRecord))
  , v0_tmmp_uploadedAt :: !(Maybe (Maybe UTCTime))
  , v0_tmmp_hiddenReason :: !(Maybe (Maybe Text))
  , v0_tmmp_atSetByPlugin :: !(Maybe (Maybe Bool))
  , v0_tmmp_nbPlayers :: !(Maybe (Maybe Int))
  }
  deriving (Show)

$(deriveSafeCopy 0 'base ''TMMapPatch_v0)

data TMMapPatch_v1
  = TMMapPatch_v1
  { v1_tmmp_tmxId :: !TMXId
  , v1_tmmp_uid :: !(Maybe Text)
  , v1_tmmp_name :: !(Maybe Text)
  , v1_tmmp_authorMedal :: !(Maybe Int)
  , v1_tmmp_authorUid :: !(Maybe (Maybe Text))
  , v1_tmmp_tags :: !(Maybe [Int])
  , v1_tmmp_currentWR :: !(Maybe (Maybe TMMapRecord))
  , v1_tmmp_uploadedAt :: !(Maybe (Maybe UTCTime))
  , v1_tmmp_hiddenReason :: !(Maybe (Maybe Text))
  , v1_tmmp_atSetByPlugin :: !(Maybe (Maybe Bool))
  , v1_tmmp_nbPlayers :: !(Maybe (Maybe Int))
  , v1_tmmp_reportedBy :: !(Maybe (Map Text (Maybe (UTCTime, Text))))
  }
  deriving (Show)

instance Migrate TMMapPatch_v1 where
  type MigrateFrom TMMapPatch_v1 = TMMapPatch_v0
  migrate (TMMapPatch_v0 tmxId uid name authorMedal authorUid tags currentWR uploadedAt hiddenReason atSetByPlugin nbPlayers) =
    TMMapPatch_v1
      { v1_tmmp_tmxId = tmxId
      , v1_tmmp_uid = uid
      , v1_tmmp_name = name
      , v1_tmmp_authorMedal = authorMedal
      , v1_tmmp_authorUid = authorUid
      , v1_tmmp_tags = tags
      , v1_tmmp_currentWR = currentWR
      , v1_tmmp_uploadedAt = uploadedAt
      , v1_tmmp_hiddenReason = hiddenReason
      , v1_tmmp_atSetByPlugin = atSetByPlugin
      , v1_tmmp_nbPlayers = nbPlayers
      , v1_tmmp_reportedBy = Nothing
      }

$(deriveSafeCopy 1 'extension ''TMMapPatch_v1)

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
  }
  deriving (Show)

instance Migrate TMMapPatch where
  type MigrateFrom TMMapPatch = TMMapPatch_v1
  migrate (TMMapPatch_v1 tmxId uid name authorMedal authorUid tags currentWR uploadedAt hiddenReason atSetByPlugin nbPlayers reportedBy) =
    TMMapPatch
      { _tmmp_tmxId = tmxId
      , _tmmp_uid = uid
      , _tmmp_name = name
      , _tmmp_authorMedal = authorMedal
      , _tmmp_authorUid = authorUid
      , _tmmp_tags = tags
      , _tmmp_currentWR = currentWR
      , _tmmp_uploadedAt = uploadedAt
      , _tmmp_hiddenReason = hiddenReason
      , _tmmp_atSetByPlugin = atSetByPlugin
      , _tmmp_nbPlayers = nbPlayers
      , _tmmp_reportedBy = reportedBy
      , _tmmp_mapType = Nothing
      }

$(deriveSafeCopy 2 'extension ''TMMapPatch)

defPatch :: TMXId -> TMMapPatch
defPatch idx = TMMapPatch idx Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
    }

data MapMonitorState_v1
  = MapMonitorState_v1
  { v1_mms_lastTmxId :: !Int
  , v1_mms_ubeatenMaps :: !(Map Int TMMap)
  , v1_mms_beatenMaps :: ![TMMap]
  }
  deriving (Show)

$(deriveSafeCopy 1 'base ''MapMonitorState_v1)

data MapMonitorState_v2
  = MapMonitorState_v2
  { v2_mms_ubeatenMaps :: !(Map TMXId TMMap)
  , v2_mms_beatenMaps :: ![TMMap]
  }
  deriving (Show)

instance Migrate MapMonitorState_v2 where
  type MigrateFrom MapMonitorState_v2 = MapMonitorState_v1
  migrate (MapMonitorState_v1 {..}) =
    MapMonitorState_v2
      { v2_mms_ubeatenMaps = Map.fromList $ map (\(tmxId, tmMap) -> (TMXId tmxId, tmMap)) $ Map.toList v1_mms_ubeatenMaps
      , v2_mms_beatenMaps = v1_mms_beatenMaps
      }

$(deriveSafeCopy 2 'extension ''MapMonitorState_v2)

data MapMonitorState
  = MapMonitorState
  { _mms_maps :: IxEntry
  }
  deriving (Show)

$(makeLenses ''MapMonitorState)

instance Migrate MapMonitorState where
  type MigrateFrom MapMonitorState = MapMonitorState_v2
  migrate (MapMonitorState_v2 {..}) =
    MapMonitorState
      { _mms_maps = IxSet.fromList (v2_mms_beatenMaps ++ Map.elems v2_mms_ubeatenMaps)
      }

$(deriveSafeCopy 3 'extension ''MapMonitorState)

patchDB :: [TMMapPatch] -> IxEntry -> IxEntry
patchDB patches db =
  foldl' go db patches
  where
    go acc p =
      case getOne (acc @= _tmmp_tmxId p) of
        Nothing -> acc
        Just m -> IxSet.updateIx (_tmmp_tmxId p) (applyPatch p m) acc

addMissingMaps :: [TMMap] -> IxEntry -> IxEntry
addMissingMaps maps db =
  foldl' go db maps
  where
    go acc m =
      case getOne (acc @= _tmm_tmxId m) of
        Nothing -> IxSet.insert m acc
        Just _ -> acc

addNewMaps :: [(TMXId, TMMap)] -> Update MapMonitorState ()
addNewMaps maps = do
  mms_maps %= addMissingMaps (snd <$> maps)

updateMaps' :: [TMMapPatch] -> Update MapMonitorState ()
updateMaps' patches = do
  mms_maps %= patchDB patches

removeMap :: TMXId -> Update MapMonitorState ()
removeMap tmxId = do
  mms_maps %= IxSet.deleteIx tmxId

replaceMaps :: [TMMap] -> Update MapMonitorState ()
replaceMaps mps = do
  mms_maps %= \db -> foldl' (\acc m -> IxSet.insert m acc) db mps

replaceMap :: TMMap -> Update MapMonitorState ()
replaceMap tmMap = do
  mms_maps %= \db -> IxSet.insert tmMap db

getAllKnownIds :: Query MapMonitorState (Set Int)
getAllKnownIds = do
  asks $ fromList . fmap (unTMXId . _tmm_tmxId) . IxSet.toList . _mms_maps

isKnownId :: TMXId -> Query MapMonitorState Bool
isKnownId tmxId = do
  asks $ isJust . getOne . (@= tmxId) . _mms_maps

getMapById :: TMXId -> Query MapMonitorState (Maybe TMMap)
getMapById tmxId = do
  asks $ getOne . (@= tmxId) . _mms_maps

getMapsByIds :: [TMXId] -> Query MapMonitorState [TMMap]
getMapsByIds ids = do
  asks $ IxSet.toList . (@* ids) . _mms_maps

getMaps :: Query MapMonitorState [TMMap]
getMaps = do
  asks $ IxSet.toList . (@= Unbeaten) . _mms_maps

getBeatenMaps :: Query MapMonitorState [TMMap]
getBeatenMaps = do
  asks $ IxSet.toList . (@= Beaten) . _mms_maps

getMapMonitorState :: Query MapMonitorState MapMonitorState
getMapMonitorState = ask

hideMap :: TMXId -> Text -> Update MapMonitorState ()
hideMap tmxId reason = do
  updateMaps' [(defPatch tmxId) { _tmmp_hiddenReason = Just $ Just reason }]

setAtSetByPlugin :: TMXId -> Maybe Bool -> Update MapMonitorState ()
setAtSetByPlugin tmxId atSetByPlugin = do
  updateMaps' [(defPatch tmxId) { _tmmp_atSetByPlugin = Just $ atSetByPlugin }]

$(makeAcidic ''MapMonitorState ['updateMaps', 'replaceMap, 'replaceMaps, 'addNewMaps, 'getMaps, 'getBeatenMaps, 'getMapMonitorState, 'hideMap, 'setAtSetByPlugin, 'getMapsByIds, 'removeMap, 'getMapById, 'isKnownId, 'getAllKnownIds])

updateMaps :: (MonadIO m) => AcidState MapMonitorState -> [TMMapPatch] -> m [TMMap]
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
