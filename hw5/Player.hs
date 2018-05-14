module Player where

import SedmaBase
import Utils

data MState = MState {
    inHand :: Hand,
    otherStats :: OtherStats
}

data OtherStats = OtherStats {
    thisPlayer :: Player,
    roundStartingPlayer :: Player,
    playedCards :: Cards
}

instance PlayerState MState where
    initState player hand = MState hand (OtherStats player A [])

    updateState trick trickWinningPlayer winningCard Nothing (MState inHand o) = MState (getCards trick inHand) (updateOtherStats o)
        where
            getCards :: Trick -> Hand -> Hand
            getCards [] inHandCards = inHandCards
            getCards (c:cs) inHandCards = if elem c inHandCards then removeItem c inHandCards else getCards cs inHandCards

            updateOtherStats :: OtherStats -> OtherStats
            updateOtherStats (OtherStats p rs cards) = OtherStats p trickWinningPlayer (merge cards trick)

    updateState x y z (Just givenCard) w = addToState (updateState x y z Nothing w) givenCard
        where
            addToState :: MState -> Card -> MState
            addToState (MState inHand o) card = MState (inHand ++ [card]) o

player :: AIPlayer MState
player [] (MState inHand _) = getCard inHand inHand
    where
        getCard :: Cards -> Hand -> Card
        getCard [] hand = hand !! 0
        getCard (x:xs) hand = if (getRank x) == R7 then getCard xs hand else x

player ((Card x winningRank): xs) (MState inHand (OtherStats thisPlayer roundStartingPlayer _)) = card
    where
        getWinningTeam :: Team
        getWinningTeam = _getWinningTeam ((Card x winningRank): xs) start start winningRank
            where
                start :: Team
                start = getTeam roundStartingPlayer

                _getWinningTeam :: Cards -> Team -> Team -> Rank -> Team
                _getWinningTeam [] currPlaying currWinning rank = currWinning
                _getWinningTeam ((Card _ rank) :xs) currPlaying currWinning winningRank =  _getWinningTeam xs (switchTeam currPlaying) winning winningRank
                    where
                        winning = if rank == winningRank || rank ==  R7 then currPlaying else currWinning

        card :: Card
        card = if (getWinningTeam == (getTeam thisPlayer)) then playerLeader else playerNotLeader
            where
                leaderCards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, winningRank == rank]
                r7Cards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, rank == R7]
                nonR7Cards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, rank != R7]
                pointsCards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, rank == RA || rank == R10]
                nonPointsCards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, rank != RA && rank != R10]

                playerLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 = r7Cards !! 0
--                     | length pointsCards > 0 = pointsCards !! 0
                    | (length leaderCards > 0) && ((getRank (leaderCards !! 0)) == R7) && ((length nonR7Cards) != 0)  = nonR7Cards !! 0
                    | otherwise = inHand !! 0

                playerNotLeader
                    | length leaderCards > 0 = leaderCards !! 0
                    | length r7Cards > 0 = r7Cards !! 0
                    | length nonPointsCards > 0 = nonPointsCards !! 0
                    | otherwise = inHand !! 0
