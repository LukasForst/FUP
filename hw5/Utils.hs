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

getWinningTeam :: Trick -> Team -> Team
getWinningTeam ((Card x winningRank): xs) roundStartingTeam = _getWinningTeam ((Card x winningRank): xs) roundStartingTeam roundStartingTeam winningRank
    where
        _getWinningTeam :: Cards -> Team -> Team -> Rank -> Team
        _getWinningTeam [] currPlaying currWinning rank = currWinning
        _getWinningTeam ((Card _ rank) :xs) currPlaying currWinning winningRank =  _getWinningTeam xs (switchTeam currPlaying) winning winningRank
            where
                winning = if rank == winningRank || rank ==  R7 then currPlaying else currWinning
