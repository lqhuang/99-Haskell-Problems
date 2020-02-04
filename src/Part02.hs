{-
 - Part 02
 - Lists, continued
 -}
module Part02 where

import Data.List (group)
import Control.Applicative ((<**>))

{-
-- Problem 11 (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
 - Only elements with duplicates are transferred as (N E) lists.
 -}
data ListItem a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map countDups $ group xs
  where countDups xs = let dups_length = length xs
                           element     = head xs in
                       if dups_length == 1
                       then Single element
                       else Multiple dups_length element
-- too long?

-- shorter
encodeModified' :: Eq a => [a] -> [ListItem a]
encodeModified' xs = map (encodeHelper . (\dups -> (length dups, head dups))) $ group xs
    where encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x

-- much shorter
encodeModified'' :: Eq a => [a] -> [ListItem a]
encodeModified'' xs = [y | x <- group xs,
                       let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

-- -- test cases:
-- main = do
--     putStrLn "Problem 11"
--     print $ encodeModified' "aaaabccaadeeee" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']


{-
 - Problem 12 (**) Decode a run-length encoded list.
 - Given a run-length code list generated as specified in problem 11.
 - Construct its uncompressed version.
-}
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
    where decodeHelper (Single x) = [x]
          decodeHelper (Multiple c x) = replicate c x

-- use uncurry. nice solution :)
decodeModified' :: [ListItem a] -> [a]
decodeModified' = concatMap (uncurry replicate . toTuple)
    where toTuple :: ListItem a -> (Int, a)
          toTuple (Single x) = (1, x)
          toTuple (Multiple n x) = (n, x)

-- a naïve solution with foldl
decodeModified'' :: [ListItem a] -> [a]
decodeModified'' = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) []

-- -- test cases:
-- main = do
--     putStrLn "Problem 12"
--     print $ decodeModified' [Multiple 4 'a', Single 'b', Multiple 2 'c',
--         Multiple 2 'a', Single 'd', Multiple 4 'e'] == "aaaabccaadeeee"


{-
 - Problem 13 (**) Run-length encoding of a list (direct solution).
 - Implement the so-called run-length encoding data compression method directly.
 - I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
 - As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
 -}
-- decodeDirect :: Eq a => [a] -> [ListItem a]
-- decodeDirect = ???

-- -- test cases:
-- main = do
--     putStrLn "Problem 13"
--     print $ encodeDirect "aaaabccaadeeee" == [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']


{-
 - Problem 14 (*) Duplicate the elements of a list.
 -}
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

-- using the list instance of `Applicative`
dupli' = (<**> [id, id])

-- -- test cases:
-- main = do
--     putStrLn "Problem 14"
--     print $ dupli' [1, 2, 3] == [1, 1, 2, 2, 3, 3]


{-
 - Problem 15 (**) Replicate the elements of a list a given number of times.
 -}
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- or, in Pointfree style:
repli' :: [a] -> Int -> [a]
repli' = flip $ concatMap . replicate

-- or, using the list monad:
repli'' :: [a] -> Int -> [a]
repli'' xs n = xs >>= replicate n

-- -- test cases:
-- main = do
--     putStrLn "Problem 15"
--     print $ repli "abc" 3 == "aaabbbccc"


{-
 - Problem 16 (**) Drop every N'th element from a list.
 -}
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = foldr (\x acc -> snd x : acc) [] $ filter (uncurry notDrop) $ zip [0..(length xs)-1] xs
    where notDrop idx _ = (idx+1) `mod` n /= 0
-- improve: dropEvery xs n = map snd . filter (uncurry notDrop) $ zip [0..(length xs)-1] xs

-- shortest of my idea
dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map snd $ filter (\(i, _) -> i `mod` n /= 0) $ zip [1..] xs

-- Using zip:
dropEvery'' xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

-- Using comprehensions
dropEvery''' :: [a] -> Int -> [a]
dropEvery''' xs n = [i | (i, c) <- (zip xs [1,2..]), (mod c n) /= 0]

-- Using unzip
dropEvery'''' :: [a] -> Int -> [a]
dropEvery'''' xs k = (snd . unzip . filter (\(i, _) -> i `mod` k /= 0) . zip [1..]) xs
-- Type signature is slightly different from the problem, but equivalent and shorter.
-- dropEvery :: Int -> [a] -> [a]
-- dropEvery k = snd . unzip . filter (\(i, _) -> i `mod` k /= 0) . zip [1..]

-- -- test cases:
-- main = do
--     putStrLn "Problem 16"
--     print $ dropEvery'''' "abcdefghik" 3
--     print $ dropEvery'''' "abcdefghik" 3 == "abdeghk"


{-
 - Problem 17 (*) Split a list into two parts; the length of the first part is given.
 - Do not use any predefined predicates.
 -}
-- split :: [a] -> Int -> ([a], [a])
-- split xs loc = (take loc xs, drop loc xs)
-- Oh, no. These should clearly be considered "predefined predicates".
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split all@(x:xs) n
    | n <= 0    = ([], all)
    | n == 1    = ([x], xs)  -- actually you could remove this condition but one more recursion
    | otherwise = (x:first, rest)
                  where (first, rest) = split xs (n - 1)

-- Another foldl solution without defining tuple extractors:
split'' :: [a] -> Int -> ([a], [a])
split'' lst n = snd $ foldl helper (0, ([], [])) lst
  where helper (i, (left, right)) x = if i >= n
                                      then (i + 1, (left, right ++ [x]))
                                      else (i + 1, (left ++ [x], right))

-- -- test cases:
-- main = do
--     putStrLn "Problem 17"
--     print $ split "abcdefghik" 3
--     print $ split "abcdefghik" 3 == ("abc", "defghik")


{-
 - Problem 18 (**) Extract a slice from a list.
 - Given two indices, i and k, the slice is the list containing the elements between
 - the i'th and k'th element of the original list (both limits included).
 - Start counting the elements with 1.
 -}
slice :: [a] -> Int -> Int -> [a]
slice xs i k | i > 0 = take (k - i + 1) $ drop (i - 1) xs
-- notice `i>0` guard condition

-- A solution using list comprehension:
slice' :: [a] -> Int -> Int -> [a]
slice' xs i k = [x | (x, j) <- zip xs [1..k], i <= j]

-- Zip, filter, unzip:
slice'' :: [a] -> Int -> Int -> [a]
slice'' xs i k = fst $ unzip $ filter ((>= i) . snd) $ zip xs [1..k]

-- test cases:
main = do
    putStrLn "Problem 18"
    print $ slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7 == "cdefg"


{-
 - Problem 19 (**) Rotate a list N places to the left.
 - Hint: Use the predefined functions length and (++).
 -}
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs shift = reverse $ reverse first ++ reverse second
  where (first, second) = if shift > 0
                          then splitAt shift xs
                          else splitAt (length xs + shift) xs

-- using cycle
rotate' :: [a] -> Int -> [a]
rotate' xs n = take len . drop (n `mod` len) . cycle $ xs
    where len = length xs

-- without mod
rotate'' :: [a] -> Int -> [a]
rotate'' xs n = take (length xs) $ drop (length xs + n) $ cycle xs

-- -- test cases:
-- main = do
--     putStrLn "Problem 19"
--     print $ rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc"
--     print $ rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"


{-
 -  Problem 20 (*) Remove the K'th element from a list.
 - (Note that not only returns the residue list, but also returns the deleted element.)
 -}
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "empty list"
removeAt k xs = (xs !! (k-1), take (k-1) xs ++ drop k xs)

-- a safe version of that
removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' n xs
    | n > 0 && n <= length xs = (Just (xs !! idx), take idx xs ++ drop n xs)
    | otherwise               = (Nothing, xs)
                                where idx = n - 1

-- another solution that also uses Maybe to indicate failure:
removeAt'' :: Int -> [a] -> (Maybe a, [a])
removeAt'' _ [] = (Nothing, [])
removeAt'' 0 xs = (Nothing, xs)
removeAt'' nr xs | nr > length xs = (Nothing, xs)
                 | otherwise = (Just (xs !! nr), fst splitted ++ (tail . snd) splitted)
                where splitted = splitAt nr xs

-- similar, point-free style:
removeAt''' :: Int -> [a] -> (a, [a])
removeAt''' n = (\(a, b) -> (head b, a ++ tail b)) . splitAt (n - 1)

-- -- test cases:
-- main = do
--     putStrLn "Problem 20"
--     print $ removeAt 2 "abcd" == ('b', "acd")
