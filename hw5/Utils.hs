module Utils where

import SedmaBase
-- following modules need to be imported for upload system
-- import SedmaGamble
-- import SedmaReplay

instance Eq Team where
    (==) AC AC = True
    (==) BD BD = True
    (==) _ _ = False

(!=) :: Eq a => a -> a -> Bool
(!=) a b = a /= b

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

getTeam :: Player -> Team
getTeam A = AC
getTeam C = AC
getTeam B = BD
getTeam D = BD

getRank :: Card -> Rank
getRank (Card suit rank) = rank

switchTeam :: Team -> Team
switchTeam AC = BD
switchTeam BD = AC
