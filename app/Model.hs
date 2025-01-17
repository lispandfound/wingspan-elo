module Model where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.List (find, groupBy, maximumBy, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
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
Game
PlayerGame
    playerId PlayerId
    gameId GameId
    bird Int
    bonus Int
    goal Int
    egg Int
    cache Int
    tucked Int
    friendship Int
    tiebreak Int
Avatar
    key Text
    name Text
|]

initializeDB :: (MonadIO m) => ReaderT SqlBackend m ()
initializeDB = runMigration migrateAll

getRankings :: (MonadIO m) => ReaderT SqlBackend m [(Key Player, Player, Ranking)]
getRankings = do
  rankingRecords <- selectList [] [Desc RankingPlayerId, Desc RankingTimestamp]
  players <- selectList [] []
  let rankings = sortOn (negate . rankingElo) . mapMaybe listToMaybe . groupBy player . map entityVal $ rankingRecords
  return $ mapMaybe (\ranking -> (\player -> (entityKey player, entityVal player, ranking)) <$> find (\player -> entityKey player == rankingPlayerId ranking) players) rankings
  where
    player (Ranking {rankingPlayerId = id1}) (Ranking {rankingPlayerId = id2}) = fromSqlKey id1 == fromSqlKey id2

getPlayerRankings :: (MonadIO m) => Key Player -> ReaderT SqlBackend m [Ranking]
getPlayerRankings key = map entityVal <$> selectList [RankingPlayerId ==. key] [Desc RankingTimestamp]

playerAvatar :: (MonadIO m) => Player -> ReaderT SqlBackend m (Maybe Avatar)
playerAvatar player = get (playerAvatarId player)

dummyRankings :: (MonadIO m) => ReaderT SqlBackend m ()
dummyRankings = do
  kiwi <- insert $ Avatar "kiwi" "Kiwi"
  kea <- insert $ Avatar "kea" "Kea"
  piwakawaka <- insert $ Avatar "piwakawaka" "Pīwakawaka"
  tui <- insert $ Avatar "tui" "Tūī"
  magnus <- insert $ Player "Magnus Birdman" "NOR" kiwi
  sofia <- insert $ Player "Sofia Wings" "UK" tui
  ada <- insert $ Player "Ada Aviary" "IND" piwakawaka
  john <- insert $ Player "John Feathers" "CAN" kea
  void . insert $ Ranking 2900.0 0.0 "2024-11-22T00:00:00Z" magnus
  void . insert $ Ranking 2900.0 0.0 "2024-11-21T00:00:00Z" magnus
  void . insert $ Ranking 2750.0 0.0 "2024-11-22T00:00:00Z" sofia
  void . insert $ Ranking 2730.0 0.0 "2024-11-22T00:00:00Z" ada
  void . insert $ Ranking 2720.0 0.0 "2024-11-22T00:00:00Z" john

avatarForKey :: (MonadIO m) => Text -> ReaderT SqlBackend m (Maybe Avatar)
avatarForKey key = listToMaybe . map entityVal <$> selectList [AvatarKey ==. key] []

avatarKeyToId :: (MonadIO m) => Text -> ReaderT SqlBackend m (Maybe (Key Avatar))
avatarKeyToId key = listToMaybe . map entityKey <$> selectList [AvatarKey ==. key] []

defaultRanking :: Key Player -> Ranking
defaultRanking = Ranking 25.0 (25.0 / 3) "NOW"

birds :: (MonadIO m) => ReaderT SqlBackend m [Entity Avatar]
birds = selectList [] []
