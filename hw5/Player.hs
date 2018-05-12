module Player where

import SedmaBase
import Utils

data State = State {
    inHand :: Hand,
    otherStats :: OtherStats
}

data OtherStats = OtherStats {
    thisPlayer :: Player,
    roundStartingPlayer :: Player,
    playedCards :: Cards
}

instance PlayerState State where
    initState player hand = State hand (OtherStats player A [])

    updateState trick trickWinningPlayer winningCard Nothing (State inHand o) = State (getCards trick inHand) (updateOtherStats o)
        where
            getCards :: Trick -> Hand -> Hand
            getCards [] inHandCards = inHandCards
            getCards (c:cs) inHandCards = if elem c inHandCards then removeItem c inHandCards else getCards cs inHandCards

            updateOtherStats :: OtherStats -> OtherStats
            updateOtherStats (OtherStats p rs cards) = OtherStats p trickWinningPlayer (merge cards trick)

    updateState x y z (Just givenCard) w = addToState (updateState x y z Nothing w) givenCard
        where
            addToState :: State -> Card -> State
            addToState (State inHand o) card = State (inHand ++ [card]) o

player :: AIPlayer State
player [] (State inHand _) = getCard inHand inHand
    where
        getCard :: Cards -> Hand -> Card
        getCard [] hand = hand !! 0
        getCard (x:xs) hand = if (getRank x) == R7 then getCard xs hand else x

player trick (State inHand (OtherStats thisPlayer roundStartingPlayer _)) = card
    where
        selectWinningCards :: Rank -> Cards -> Cards
        selectWinningCards _ [] = []
        selectWinningCards targetRank (x:xs)
            | targetRank == (getRank x) = [x] ++ (selectWinningCards targetRank xs)
            | R7 == (getRank x) = (selectWinningCards targetRank xs) ++ [x]
            | otherwise = (selectWinningCards targetRank xs)

        getWinningTeam :: Team
        getWinningTeam = _getWinningTeam trick start start (getRank (trick !! 0))
            where
                start :: Team
                start = getTeam roundStartingPlayer

                _getWinningTeam :: Cards -> Team -> Team -> Rank -> Team
                _getWinningTeam [] currPlaying currWinning rank = currWinning
                _getWinningTeam ((Card _ rank) :xs) currPlaying currWinning winningRank =  _getWinningTeam xs (switchTeam currPlaying) winning winningRank
                    where
                        winning = if rank == winningRank || rank ==  R7 then currPlaying else currWinning

        selected :: Cards
        selected = selectWinningCards (getRank (trick !! 0)) inHand

        card :: Card
        card
            | length selected == 0 = inHand !! 0
            | ((getRank (selected !! 0)) == R7) && (getWinningTeam == (getTeam thisPlayer)) && ((length nonR7Cards) != 0)  = nonR7Cards !! 0
            | otherwise = selected !! 0
            where
                nonR7Cards = take 4 [(Card suit rank) | (Card suit rank) <- inHand, rank != R7]
