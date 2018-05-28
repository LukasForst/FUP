type Map a b = [(a, b)]

initmap :: Map a b
initmap = []

find :: (Eq a) => Map a b -> a -> Maybe b
find [] _ = Nothing
find ((key, value):xs) toFindKey = if key == toFindKey then Just value else find xs toFindKey

delete :: (Eq a) => Map a b -> a -> Map a b
delete [] _ = []
delete ((key, value):xs) toDeleteKey = if key == toDeleteKey then xs else [(key, value)] ++ (delete xs toDeleteKey)

add :: Eq(a) => Map a b -> a -> b -> Map a b
add map key value = (delete map key) ++ [(key, value)]


data Token = Mult | Add | Num Integer deriving (Show, Eq)

interp :: [Token] -> [Integer] -> Integer
interp ((Num x):tl) s = interp tl (x:s)
interp ((Add):tl) (a:b:ts) = interp tl (a + b : ts)
interp ((Mult):tl) (a:b:ts) = interp tl (a * b : ts)
interp _ s = head s

parse :: String -> Token
parse "*" = Mult
parse "+" = Add
parse x = Num (read x :: Integer)

split :: String -> Char -> [String]
split [] _ = [""]
split (x:xs) c = let rest = split xs c in if x == c then "" : rest else (x : head rest) : tail rest

calculator :: String -> Integer
calculator s = interp (map parse (split s ' ')) []

main :: IO()
main = do 
    s <- getLine
    putStrLn (show (calculator s))
    main