import Utilities
import SedmaDatatypes

replay :: Cards -> Maybe Winner
replay [] = Nothing
replay cards = _replay cards ((AC, 0, 0), (BD, 0, 0)) AC
    where
        _replay :: Cards -> ((Team, Int, Int),(Team, Int, Int)) -> Team -> Maybe Winner
        _replay cards evaluatedTeams teamOnMove
            | (mod (length cards) 4) != 0 = Nothing
            | length cards == 0 = getWinner evaluatedTeams
            | otherwise = _replay nextCards nextEvaluated nextTeamOnMove
                where
                    (thisGameCards, nextCards) = getOneGameCards cards
                    (nextTeamOnMove, s, tr) = playOne thisGameCards teamOnMove
                    nextEvaluated = addToTeams evaluatedTeams (nextTeamOnMove, s, tr)


playOne :: Cards -> Team -> (Team, Int, Int)
playOne cards team = _playOne cards (switchTeam team) team (getRank (cards !! 0)) 1
    where
        _playOne :: Cards -> Team -> Team -> Rank -> Int -> (Team, Int, Int)
        _playOne cards currentTeam trickTeam trickRank stage
            | stage == -1 = (trickTeam, evaluateOneRoundCards cards, 1)
            | otherwise = _playOne cards (switchTeam currentTeam) leader trickRank (stage - 1)
                where
                    leader = if (getRank (cards !! stage)) == trickRank then currentTeam else trickTeam

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
