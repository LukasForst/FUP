module SedmaDatatypes where

data Suit = Heart | Diamond | Spade | Club deriving Show
data Rank = R7 | R8 | R9 | R10 | RJ | RQ | RK | RA deriving Show
data Card = Card Suit Rank deriving Show
type Cards = [Card]

data Team = AC | BD deriving Show
data Points = One | Two | Three deriving Show
type Winner = (Team, Points)

instance Eq Team where
    (==) AC AC = True
    (==) BD BD = True
    (==) _ _ = False

-- [Card Club RA,Card Club RK,Card Club RQ,Card Club RJ,Card Club R10,Card Club R9,Card Club R7,Card Club R8,Card Spade RA,Card Spade RK,Card Spade RQ,Card Spade RJ,Card Spade R10,Card Spade R9,Card Spade R7,Card Spade R8,Card Diamond RA,Card Diamond RK,Card Diamond RQ,Card Diamond RJ,Card Diamond R7,Card Diamond R9,Card Diamond R8,Card Diamond R10,Card Heart RJ,Card Heart RK,Card Heart RQ,Card Heart RA,Card Heart R7,Card Heart R9,Card Heart R8,Card Heart R10]
