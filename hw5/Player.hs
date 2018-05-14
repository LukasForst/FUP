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
player [] (MState inHand (OtherStats _ _ _ played)) = finalize mostPlayedRankInHand
    where
        non7Cards = filter (\ (Card _ rank) -> rank != R7) inHand
        mostPlayedRankInHand :: (Maybe Rank)
        mostPlayedRankInHand = _mostPlayedRankInHand played inHand 0 R7
            where
                _mostPlayedRankInHand :: Cards -> Cards -> Int -> Rank -> (Maybe Rank)
                _mostPlayedRankInHand [] _ _ _ = Nothing
                _mostPlayedRankInHand _ [] _ currentMaxRank = (Just currentMaxRank)
                _mostPlayedRankInHand played ((Card _ nextRank):xs) currentMax currentMaxRank
                    | currentCount > currentMax = _mostPlayedRankInHand played xs currentCount nextRank
                    | otherwise = _mostPlayedRankInHand played xs currentMax currentMaxRank
                    where
                        currentCount = length (filter (\ (Card _ rank) -> rank == nextRank) played)

        finalize Nothing = if length non7Cards > 0 then non7Cards !! 0 else inHand !! 0
        finalize (Just value) = if length filtered > 0 then filtered !! 0 else finalize Nothing
            where
                filtered = filter (\ (Card _ rank) -> rank == value) inHand

player ((Card x winningRank): xs) (MState inHand (OtherStats _ thisTeam roundStartingTeam _)) = card
    where
        card :: Card
        card = if ((getWinningTeam ((Card x winningRank): xs) roundStartingTeam) == thisTeam) then playerLeader else playerNotLeader
            where
                leaderCards = filter (\ (Card _ rank) -> rank == winningRank) inHand
                r7Cards = filter (\ (Card _ rank) -> rank == R7) inHand
                nonR7Cards = filter (\ (Card _ rank) -> rank != R7) inHand
                rewardedCards = filter (\ (Card _ rank) -> rank == RA || rank == R10) inHand
                nonPointsCards = filter (\ (Card _ rank) -> rank != R10 && rank != RA) inHand

                playerLeader
                    | length rewardedCards > 0 = rewardedCards !! 0
                    | length nonR7Cards > 0 = nonR7Cards !! 0
                    | otherwise = inHand !! 0

                playerNotLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 = r7Cards !! 0
                    | length nonPointsCards > 0 = nonPointsCards !! 0
                    | otherwise = inHand !! 0
