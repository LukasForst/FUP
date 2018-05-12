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

sillyPlayer :: AIPlayer State
sillyPlayer trick (State inHand _) = inHand !! 0

player :: AIPlayer State
player [] (State inHand _) = getCard inHand inHand
    where
        getCard :: Cards -> Hand -> Card
        getCard [] hand = hand !! 0
        getCard ((Card suit rank):xs) hand = if rank == R7 then getCard xs hand else (Card suit rank)

player trick (State inHand _) = if length inHand > 1 then inHand !! 1 else inHand !! 0

