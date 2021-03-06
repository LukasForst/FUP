module GreedyPlayer where

import SedmaBase
import Player
import Utils

greedyPlayer :: AIPlayer MState
greedyPlayer [] (MState inHand (OtherStats _ _ _ played)) = card
    where
        mostPlayedRank :: (Maybe Rank, Int)
        mostPlayedRank = _mostPlayedRankInHand played inHand 0 Nothing
            where
                _mostPlayedRankInHand :: Cards -> Cards -> Int -> (Maybe Rank) -> (Maybe Rank, Int)
                _mostPlayedRankInHand [] _ _ _ = (Nothing, 0)
                _mostPlayedRankInHand _ [] count currentMaxRank = (currentMaxRank, count)
                _mostPlayedRankInHand played ((Card _ nextRank):xs) _ Nothing = _mostPlayedRankInHand played xs (length (filter (\ (Card _ rank) -> rank == nextRank) played)) (Just nextRank)
                _mostPlayedRankInHand played ((Card _ nextRank):xs) currentMax (Just currentMaxRank)
                    | currentCount > currentMax = _mostPlayedRankInHand played xs currentCount (Just nextRank)
                    | otherwise = _mostPlayedRankInHand played xs currentMax (Just currentMaxRank)
                        where
                            currentCount = length (filter (\ (Card _ rank) -> rank == nextRank) played)

        r7Cards = filter (\ (Card _ rank) -> rank == R7) inHand
        nonR7Cards = filter (\ (Card _ rank) -> rank != R7) inHand
        scoringCards = filter (\ (Card _ rank) -> rank == RA || rank == R10) inHand
        filteredByMostPlayed = case mostPlayedRank of
                                (Just value, count) -> if count >= 3 then  filter (\ (Card _ rank) -> rank == value) inHand else []
                                (Nothing, _) -> []

        card :: Card
        card
            | length filteredByMostPlayed > 0 = filteredByMostPlayed !! 0
            | length scoringCards > 0 = scoringCards !! 0
            | length nonR7Cards > 0 = nonR7Cards !! 0
            | otherwise = inHand !! 0

greedyPlayer ((Card x winningRank): xs) (MState inHand (OtherStats _ thisTeam roundStartingTeam _)) = card
    where
        card :: Card
        card = if ((getWinningTeam ((Card x winningRank): xs) roundStartingTeam) == thisTeam) then playerLeader else playerNotLeader
            where
                leaderCards = filter (\ (Card _ rank) -> rank == winningRank) inHand
                r7Cards = filter (\ (Card _ rank) -> rank == R7) inHand
                nonR7Cards = filter (\ (Card _ rank) -> rank != R7) inHand
                scoringCards = filter (\ (Card _ rank) -> rank == RA || rank == R10) inHand
                nonPointsCards = filter (\ (Card _ rank) -> rank != R10 && rank != RA) inHand

                playerLeader
                    | length scoringCards > 0 = scoringCards !! 0
                    | length nonR7Cards > 0 = nonR7Cards !! 0
                    | otherwise = inHand !! 0

                playerNotLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 = r7Cards !! 0
                    | length nonPointsCards > 0 = nonPointsCards !! 0
                    | otherwise = inHand !! 0
