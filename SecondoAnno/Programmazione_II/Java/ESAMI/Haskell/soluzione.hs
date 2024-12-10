-- Esercizio 1
halve x = splitAt (div (length x) 2) x

-- Esercizio 2
third x = head(tail(tail x))
third x = x !! 2
third (_:_:x:_) = x

-- Esercizio 3
sumdown num = if num>0 then num + sumdown(num-1) else 0

-- Esercizio 4
and [] = True
and (x:xs) = x && and xs

concat [] = True
concat (x:xs) = x ++ concat xs

(x:_) !! 0 = xs
(_:xs) !! n = xs !! (n-1)

elem _ [] = False
elem x (y:ys) = if x==y then True else elem x ys

-- Esercizio 5
count _ [] = 0
count x (y:ys) = if x==y then 1 + count x ys else count x ys

-- Esercizio 6
all _ [] = True
all cond (x:xs)
    | cond x = all cond xs
    | otherwise = False

any _ [] = False
any cond (x:xs)
    | cond x = True
    | otherwise = any cond xs

takeWhile _ [] = []
takeWhile cond (x:xs)
    | cond x = x : takeWhile cond xs
    | otherwise = []

dropWhile _ [] = []
dropWhile cond (x:xs)
    | cond x = dropWhile cond xs
    | otherwise = x:xs

-- Esercizio 7
dec2int = foldl(\x y -> 10*x + y) 0

-- Esercizio 8
altMap _ _ [] = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs