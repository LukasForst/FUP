module Utilities where
import SedmaDatatypes

instance Eq Rank where
    (==) R7 R7 = True
    (==) R8 R8 = True
    (==) R9 R9 = True
    (==) R10 R10 = True
    (==) RJ RJ = True
    (==) RQ RQ = True
    (==) RK RK= True
    (==) RA RA = True
    (==) _ _ = False


getRank :: Card -> Rank
getRank (Card suit rank) = rank

switchTeam :: Team -> Team
switchTeam team = if team == AC then BD else AC

getOneGameCards :: Cards -> (Cards, Cards)
getOneGameCards cards = (one, rest)
    where
        one = take 4 cards
        rest = drop 4 cards

addToTeams :: ((Team, Int, Int), (Team, Int, Int)) -> (Team, Int, Int) -> ((Team, Int, Int), (Team, Int, Int))
addToTeams ((t1, s1, tr1), (t2, s2, tr2)) (t, s, tr) =
    if t1 == t
        then (addToTeam (t1, s1, tr1) (t, s, tr), (t2, s2, tr2))
    else ((t1, s1, tr1), addToTeam (t2, s2, tr2) (t, s, tr))

addToTeam :: (Team, Int, Int) -> (Team, Int, Int) -> (Team, Int, Int)
addToTeam (t1, score, tricsWon) (t2, scoreToAdd, tricsWonToAdd) = (t1, (score + scoreToAdd), (tricsWon + tricsWonToAdd))

(!=) :: Eq a => a -> a -> Bool
(!=) a b = a /= b
