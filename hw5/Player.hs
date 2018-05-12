-- Possible gotcha:
--     missing -
--          data Team = AC | BD deriving (Eq, Show)

module Player where

import SedmaBase

(!=) :: Eq a => a -> a -> Bool
(!=) a b = a /= b

data State = State {
    inHand :: Hand
}

instance PlayerState State where
    initState player hand = State hand

    updateState trick player currentWinningCard Nothing (State inHand) = (State (getCards trick inHand))
        where
            removeItem :: Eq a => a -> [a] -> [a]
            removeItem _ [] = []
            removeItem x (y:ys) | x == y = removeItem x ys
                                | otherwise = y : removeItem x ys

            getCards :: Trick -> Hand -> Hand
            getCards [] inHandCards = inHandCards
            getCards (c:cs) inHandCards = if elem c inHandCards then removeItem c inHandCards else getCards cs inHandCards

    updateState trick player currentWinningCard (Just givenCard) state = addToState (updateState trick player currentWinningCard Nothing state) givenCard
        where
            addToState :: State -> Card -> State
            addToState (State inHand) card = (State (inHand ++ [card]))

sillyPlayer :: AIPlayer State
sillyPlayer trick (State inHand) = inHand !! 0

player :: AIPlayer State
player [] (State inHand) = getCard inHand inHand
    where
        getCard :: Cards -> Hand -> Card
        getCard [] hand = hand !! 0
        getCard ((Card suit rank):xs) hand = if rank == R7 then getCard xs hand else (Card suit rank)

player trick (State inHand) = if length inHand > 1 then inHand !! 1 else inHand !! 0

