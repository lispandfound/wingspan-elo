module Wingspan (updateRankings, outcome) where

import Control.Monad (guard)
import Data.List (elemIndex, sortOn)
import Data.Maybe
import Model
import Ratings.Elo
import Ratings.Types

score :: PlayerGame -> Double
score (PlayerGame _ _ bird bonus goal egg cache tucked friendship tiebreak) =
  (sum . map fromIntegral) [bird, bonus, goal, egg, cache, tucked, friendship, tiebreak] + (fromIntegral tiebreak / 100)

outcome :: [PlayerGame] -> [PlayerId]
outcome = map playerGamePlayerId . sortOn score

wingspanElo :: [(Double, Double)] -> Double -> Double
wingspanElo opps = _unRating . _unElo . updateEloMatch 10 (map (\(elo, score) -> ((Elo . Rating) elo, Score score)) opps) . Elo . Rating

updateRankings :: [PlayerId] -> [Ranking] -> [Ranking]
updateRankings outcome rankings = map updateRanking rankings
  where
    updateRanking me =
      me
        { rankingElo =
            wingspanElo
              ( mapMaybe
                  ( \opp -> do
                      indMe <- elemIndex (rankingPlayerId me) outcome
                      indOpp <- elemIndex (rankingPlayerId opp) outcome
                      guard (rankingPlayerId opp /= rankingPlayerId me)
                      pure (rankingElo opp, fromIntegral . signum $ indMe - indOpp)
                  )
                  rankings
              )
              (rankingElo me)
        }
