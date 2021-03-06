module Zmodules.Zlists
( intersperse
, concat'
, intercalate
, transpose
, head'
, tail'
, concatMap'
, and'
, or'
, any'
, all'
, iterate'
, splitAt'
, takeWhile'
, dropWhile'
, span'
, break'
, sort'
, group'
, init'
, tails'
, inits'
, reverse'
, search'
, isInfixOf'
, isPrefixOf'
, isSuffixOf'
, elem'
, notElem'
, partition'
, find'
, elemIndex'
, gotoIndex'
, elemIndices'
, findIndex'
, findIndices'
, zipWith3'
, zip4'
, lines'
, unlines'
, words'
, unwords'
, nub'
, eraseAll'
, delete'
, diff'
, union'
, intersec'
, insert'
) where

head' :: [a] -> a
head' [] = error "calling head' with empty list"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "calling tail with empty list"
tail' (_:xs) = xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' ((x:xs):xss) = x : concat' (xs : xss)
concat' ([]:xss) = concat' xss

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse y (x:xs) = x : y : intersperse y xs

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [[]] = []
intercalate _ [] = []
intercalate x xs = concat' $ intersperse x xs

transpose :: [[a]] -> [[a]]
transpose [[]] = [[]]
transpose ([]:xss) = []
transpose rows = (map head' rows) : transpose (map tail' rows)

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f xs = concat' $ map f xs

and' :: [Bool] -> Bool
and' = foldr (\x acc -> if x && acc then True else False) True

or' :: [Bool] -> Bool
or' = foldr (\x acc -> if acc || x then True else False) True

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and $ map f xs

any' :: (a -> Bool) -> [a] -> Bool
any' f xs = or $ map f xs

iterate' :: (a -> a) -> a -> [a]
iterate' f v = v : iterate' f (f v)

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' _ [] = ([], [])
splitAt' v (x:xs)
     | v < 0 = error "Calling splitAt with under 0 value"
     | otherwise = ((x:a), b)
     where (a, b) = splitAt' (v - 1) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = case f x of True -> x : takeWhile' f xs
                                  False -> []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) = case f x of True -> dropWhile' f xs
                                  False -> x:xs

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f xs = (takeWhile' f xs, dropWhile' f xs)

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' f xs = span' (not . f) xs

sort' :: (Ord a) => [a] -> [a]
sort' [] = []
sort' (x:xs) = let smallerV = sort' [y | y <- xs, y <= x]
                   biggerV  = sort' [z | z <- xs, z > x]
               in smallerV ++ [x] ++ biggerV

group' :: (Eq a, Ord a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = let head'' = x
                   in [y | y <- (x:xs), y == head''] : group' [z | z <- (x:xs), z > head'']

init' :: [a] -> [a]
init' [] = error "List must be non empty"
init' [x] = []
init' (x:xs) = x : init' xs

tails' :: [a] -> [[a]]
tails' [] = []
tails' [x] = [x] : [] : tails' []
tails' xs = xs : tails' (tail' xs)

inits' :: [a] -> [[a]]
inits' xs = tails' xs

reverse' :: [a] -> [a]
reverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack = foldl isSame False (tails' haystack)
    where isSame = (\acc x -> if take lengthNeedle x == needle then True else acc)
          lengthNeedle = length needle

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' needle haystack = search' needle haystack

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = False
isPrefixOf' needle haystack = (take needleLen haystack) == needle
    where needleLen = length needle

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' needle haystack = isPrefixOf' (reverse' needle) (reverse' haystack)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' needle (x:haystack)
  | x == needle = True
  | otherwise = elem' needle haystack

notElem' :: (Eq a) => a -> [a] -> Bool
notElem' needle haystack = not . elem' needle $ haystack

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f (x:xs)
  | pred == True = (x:a, b)
  | pred == False = (a, x:b)
  where (a, b) = partition' f xs
        pred = f x

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' f (x:xs) = case f x of True -> Just x
                             False -> find' f xs

-- | get index --

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' _ [] = Nothing
elemIndex' needle haystack = elemIndex'' needle haystack 0
    where elemIndex'' _ [] _ = Nothing
          elemIndex'' needle (h:haystack) idx
            | h == needle = Just idx
            | otherwise = elemIndex'' needle haystack (idx + 1)

-- | goto certain index -- 

gotoIndex' :: Int -> [a] -> [a]
gotoIndex' _ [] = []
gotoIndex' 0 (x:xs) = xs
gotoIndex' idx (x:xs) = gotoIndex' (idx - 1) xs

-- | Find a list of index --

elemIndices'' :: (Eq a) => a -> Int -> [a] -> [Int]
elemIndices'' _ _ [] = []
elemIndices'' needle index (h:haystack)
  | needle == h = index : elemIndices'' needle (index + 1) haystack
  | otherwise = elemIndices'' needle (index + 1) haystack

elemIndices' :: (Eq a) => a ->  [a] -> [Int]
elemIndices' needle haystack = elemIndices'' needle 0 haystack

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f xs = findIndex'' f xs 0
    where findIndex'' _ [] _ = Nothing
          findIndex'' f (x:xs) idx
            | f x == True = Just idx
            | otherwise = findIndex'' f xs (idx + 1)

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' f xs = findIndices'' f xs 0
    where findIndices'' _ [] _ = []
          findIndices'' f (x:xs) idx
            | f x = idx : findIndices'' f xs (idx + 1)
            | otherwise = findIndices'' f xs (idx + 1)

zipWith3' :: (a -> a -> a -> b) -> [a] -> [a] -> [a] -> [b]
zipWith3' f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs
zipWith3' _ _ _ _ = []

zip4' :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4' (w:ws) (x:xs) (y:ys) (z:zs) = (w, x, y, z) : zip4' ws xs ys zs
zip4' _ _ _ _ = []

lines' :: String -> [String]
lines' [] = []
lines' xs = takeWhile' (\x -> x /= '\n') xs : lines' (dropWhile'' xs '\n')
    where dropWhile'' [] _ = []
          dropWhile'' (x:xs) c
            | c == x = xs
            | otherwise = dropWhile'' xs c

unlines' :: [String] -> String
unlines' [] = []
unlines' (x:xs) = x ++ "\n" ++ unlines xs

words' :: String -> [String]
words' [] = []
words' xs = takeWhile' (\x -> x /= '\n' && x /= '\t' && x /= ' ') xs : words' (dropWhile'' xs)
    where dropWhile'' [] = []
          dropWhile'' (x:xs)
            | x == '\n' || x == '\t' || x == ' ' = goto'' xs
            | otherwise = dropWhile'' xs
          goto'' [] = []
          goto'' (x:xs)
            | x == '\n' || x == '\t' || x == ' ' = goto'' xs
            | otherwise = (x:xs)

unwords' :: [String] -> String
unwords' [] = []
unwords' (x:xs) = x ++ " " ++ unwords' xs

eraseAll' :: (Eq a) => a -> [a] -> [a]
eraseAll' _ [] = []
eraseAll' needle (h:haystack)
  | needle == h = eraseAll' needle haystack
  | otherwise = h : eraseAll' needle haystack

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (eraseAll' x xs)

delete' :: (Eq a) => a -> [a] -> [a]
delete' _ [] = []
delete' needle (h:haystack)
  | needle == h = haystack
  | otherwise = h : delete' needle haystack

diff' :: (Eq a) => [a] -> [a] -> [a]
diff' xs (y:ys) = diff' (delete' y xs) ys
diff' xs [] = xs

union' :: (Eq a) => [a] -> [a] -> [a]
union' xs ys = xs ++ union'' xs ys
    where union'' _ [] = []
          union'' xs (y:ys)
            | find' (\x -> x == y) xs == Nothing = y : union'' xs ys
            | otherwise = union'' xs ys

intersec' :: (Eq a) => [a] -> [a] -> [a]
intersec' (x:xs) ys
  | find' (\y -> y == x) ys == Nothing = intersec' xs ys
  | otherwise = x : intersec' xs ys
intersec' [] _ = []

insert' :: (Ord a) => a -> [a] -> [a]
insert' needle [] = needle : []
insert' needle (h:haystack)
  | needle < h = needle : h : haystack
  | otherwise = h : insert' needle haystack
