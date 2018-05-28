module Player where

import SedmaBase
import Utils

data MState = MState {
    inHand :: Hand,
    otherStats :: OtherStats
}

data OtherStats = OtherStats {
    thisPlayer :: Player,
    thisTeam :: Team,
    roundStartingTeam :: Team,
    playedCards :: Cards
}

instance PlayerState MState where
    initState player hand = MState hand (OtherStats player (getTeam player) AC [])

    updateState trick trickStartingPlayer winningCard Nothing (MState inHand o) = MState (getCards trick inHand) (updateOtherStats o)
        where
            getCards :: Trick -> Hand -> Hand
            getCards [] inHandCards = inHandCards
            getCards (c:cs) inHandCards = if elem c inHandCards then removeItem c inHandCards else getCards cs inHandCards

            updateOtherStats :: OtherStats -> OtherStats
            updateOtherStats (OtherStats p team rs cards) = OtherStats p team (getWinningTeam trick (getTeam trickStartingPlayer)) (cards ++ trick)

    updateState x y z (Just givenCard) w = addToState (updateState x y z Nothing w) givenCard
        where
            addToState :: MState -> Card -> MState
            addToState (MState inHand o) card = MState (inHand ++ [card]) o

player :: AIPlayer MState
player [] (MState inHand (OtherStats _ _ _ played)) = card
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
                                (Just value, count) -> if count >= 2 then  filter (\ (Card _ rank) -> rank == value) inHand else []
                                (Nothing, _) -> []

        card :: Card
        card
            | length filteredByMostPlayed > 0 = filteredByMostPlayed !! 0
            | length nonR7Cards > 0 = nonR7Cards !! 0
            | otherwise = inHand !! 0

player ((Card x winningRank): xs) (MState inHand (OtherStats _ thisTeam roundStartingTeam played)) = card
    where
        card :: Card
        card = if ((getWinningTeam ((Card x winningRank): xs) roundStartingTeam) == thisTeam) then playerLeader else playerNotLeader
            where
                leaderCards = filter (\ (Card _ rank) -> rank == winningRank) inHand
                r7Cards = filter (\ (Card _ rank) -> rank == R7) inHand
                nonR7Cards = filter (\ (Card _ rank) -> rank != R7) inHand
                scoringCards = filter (\ (Card _ rank) -> rank == RA || rank == R10) inHand
                nonPointsCards = filter (\ (Card _ rank) -> rank != R10 && rank != RA) inHand

                winningPointsInTrick = length (filter (\ (Card _ rank) -> rank == RA || rank == R10) ((Card x winningRank): xs))

                playerLeader
                    | length scoringCards > 0 && (length ((Card x winningRank): xs)) >= 2 = scoringCards !! 0
                    | length nonR7Cards > 0 = nonR7Cards !! 0
                    | otherwise = inHand !! 0

                playerNotLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 && (winningPointsInTrick >= 1) = r7Cards !! 0
                    | length nonPointsCards > 0 = nonPointsCards !! 0
                    | otherwise = inHand !! 0
