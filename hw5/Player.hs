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
player [] (MState inHand _) = getCard inHand inHand
    where
        getCard :: Cards -> Hand -> Card
        getCard [] hand = hand !! 0
        getCard ((Card x rank):xs) hand = if rank == R7 then getCard xs hand else (Card x rank)

player ((Card x winningRank): xs) (MState inHand (OtherStats _ thisTeam roundStartingTeam _)) = card
    where
        card :: Card
        card = if ((getWinningTeam ((Card x winningRank): xs) roundStartingTeam) == thisTeam) then playerLeader else playerNotLeader
            where
                generateCards :: (Rank -> Bool) -> Cards
                generateCards condition = take 4 [(Card suit rank) | (Card suit rank) <- inHand, condition rank]

                leaderCards = generateCards (\ x -> x == winningRank)
                r7Cards = generateCards (\ x -> x == R7)
                nonR7Cards = generateCards (\ x -> x != R7)
                pointsCards = generateCards (\ x -> x == R10 || x == RA)
                nonPointsCards = generateCards (\ x -> x != R10 && x != RA)

                playerLeader
                    | length pointsCards > 0 = pointsCards !! 0
                    | length nonR7Cards > 0 = nonR7Cards !! 0
                    | otherwise = inHand !! 0

                playerNotLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 = r7Cards !! 0
                    | length nonPointsCards > 0 = nonPointsCards !! 0
                    | otherwise = inHand !! 0
