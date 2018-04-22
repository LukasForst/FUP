split [] = ([],[])
split [x] = ([x],[x])
split (x1:x2:xs) = let (a, b) = split xs in (x1:a, x2:b)


merge ls1 [] = ls1
merge [] ls1 = ls1
merge (x:ls1) (y:ls2) = if x <= y then x : y : (merge ls1 ls2) else y : x : merge ls2 ls1

mergesort [] = []
mergesort [x] = [x]
mergesort ls = let (f, s) = (split ls) in merge (mergesort f) (mergesort s)

data Color = Red | White | Blue deriving (Eq, Show)


