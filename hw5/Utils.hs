module Utils where

import SedmaBase

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
