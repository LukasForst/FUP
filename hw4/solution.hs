-- import Utilities
import SedmaDatatypes

replay :: Cards -> Maybe Winner
replay [] = Nothing
replay cards = if dataOk then _replay cards ((AC, 0, 0), (BD, 0, 0)) AC else Nothing
    where
        dataOk = areDataCorrect cards
        _replay :: Cards -> ((Team, Int, Int),(Team, Int, Int)) -> Team  -> Maybe Winner
        _replay cards evaluatedTeams teamOnMove
            | length cards == 0 = getWinner (addToTeams evaluatedTeams (teamOnMove, 10, 0))
            | otherwise = _replay nextCards nextEvaluated nextTeamOnMove
                where
                    (thisGameCards, nextCards) = getOneGameCards cards
                    (nextTeamOnMove, s, tr) = playOne thisGameCards teamOnMove
                    nextEvaluated = addToTeams evaluatedTeams (nextTeamOnMove, s, tr)


areDataCorrect :: Cards -> Bool
areDataCorrect cards
    | (mod (length cards) 4) != 0 = False
    | length cards != 32 = False
    | otherwise = True

playOne :: Cards -> Team -> (Team, Int, Int)
playOne cards team = _playOne cards (switchTeam team) team (getRank (cards !! 0)) 1
    where
        _playOne :: Cards -> Team -> Team -> Rank -> Int -> (Team, Int, Int)
        _playOne cards currentTeam trickTeam trickRank stage
            | stage == -1 = (trickTeam, evaluateOneRoundCards cards, 1)
            | otherwise = _playOne cards (switchTeam currentTeam) leader trickRank (stage - 1)
                where
                    currRank = getRank (cards !! stage)
                    leader = if currRank == trickRank || currRank == R7 then currentTeam else trickTeam

evaluateOneRoundCards :: Cards -> Int
evaluateOneRoundCards cards = _countCards cards 0
    where
        _countCards :: Cards -> Int -> Int
        _countCards cards score
            | length cards == 0 = score
            | (getRank (cards !! 0)) == RA || (getRank (cards !! 0)) == R10 = _countCards (drop 1 cards) (score + 10)
            | otherwise = _countCards (drop 1 cards) score

getWinner :: ((Team, Int, Int), (Team, Int, Int)) -> Maybe Winner
getWinner ((t1, score1, tricsWon1), (t2, score2, tricsWon2)) = eval
    where
        points :: Int -> Int -> Points
        points tricsWon score
            | tricsWon == 0 = Three
            | score == 0 = Two
            | otherwise = One
        eval
            | score1 == score2 = Nothing
            | score1 > score2 = Just (t1, (points tricsWon2 score2))
            | score1 < score2 = Just (t2, (points tricsWon2 score1))


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
