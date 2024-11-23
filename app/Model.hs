module Model where


import Data.Text (Text)
import Control.Monad (void)
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)
import GHC.Int (Int64)
import Data.Ord (comparing)
import Data.List (sortOn, groupBy, maximumBy, find)
import Data.Maybe (mapMaybe, listToMaybe)
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
    name Text
    country Text
    avatarId AvatarId
    deriving Show
Ranking
    elo Double
    stddev Double
    timestamp Text
    playerId PlayerId
    deriving Show
Avatar
    key Text
    name Text
|]



initializeDB :: MonadIO m =>  ReaderT SqlBackend m ()
initializeDB  = runMigration migrateAll
  
getRankings :: MonadIO m =>  ReaderT SqlBackend m [(Key Player, Player, Ranking)]
getRankings  = do
  rankingRecords <- selectList [] []
  players <- selectList [] []
  let rankings =  sortOn (negate . rankingElo) . map (maximumBy (comparing rankingTimestamp)) . groupBy player . map entityVal $ rankingRecords
  return $ mapMaybe (\ranking -> fmap (\player -> (( entityKey) player, entityVal player, ranking)) $ find (\player -> entityKey player == rankingPlayerId ranking) players) rankings
  
  where
    player (Ranking {rankingPlayerId = id1}) (Ranking {rankingPlayerId = id2}) = id1 == id2

getPlayerRankings :: MonadIO m => Key Player -> ReaderT SqlBackend m [Ranking]
getPlayerRankings key = map entityVal <$> selectList [RankingPlayerId ==. key] [Desc RankingTimestamp]

playerAvatar :: MonadIO m => Player -> ReaderT SqlBackend m (Maybe Avatar)
playerAvatar player = get (playerAvatarId player)

dummyRankings :: MonadIO m =>  ReaderT SqlBackend m ()
dummyRankings = do
  kiwi <- insert $ Avatar "kiwi" "Kiwi"
  kea <- insert $ Avatar "kea" "Kea"
  piwakawaka <- insert $ Avatar "piwakawaka" "Pīwakawaka"
  tui <- insert $ Avatar "tui" "Tūī"
  magnus <- insert $ Player "Magnus Birdman" "NOR" kiwi
  sofia <- insert $ Player "Sofia Wings" "UK" tui 
  ada <- insert $ Player "Ada Aviary" "IND" piwakawaka
  john <- insert $ Player "John Feathers" "CAN" kea
  void . insert $ Ranking 2900.0 0.0 "2024-11-22T00:00:00" magnus
  void . insert $ Ranking 2900.0 0.0 "2024-11-21T00:00:00" magnus
  void . insert $ Ranking 2750.0 0.0 "2024-11-22T00:00:00" sofia
  void . insert $ Ranking 2730.0 0.0 "2024-11-22T00:00:00" ada
  void . insert $ Ranking 2720.0 0.0 "2024-11-22T00:00:00" john

avatarForKey :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe Avatar)
avatarForKey key = listToMaybe . map entityVal <$> selectList [AvatarKey ==. key] []

avatarKeyToId :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe (Key Avatar))
avatarKeyToId key = listToMaybe . map entityKey <$> selectList [AvatarKey ==. key] []

defaultRanking :: Key Player -> Ranking
defaultRanking player = Ranking 25.0 (25.0 / 3) "NOW" player


birds :: MonadIO m => ReaderT SqlBackend m [Entity Avatar]
birds = selectList [] []
