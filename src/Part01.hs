{-
 - Part 01
 - Lists
 -}
module Part01 where

import Data.List (group)


{-
 - Problem 1: Find the last element of a list.
 -}
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- -- test cases:
-- main = do
--     putStrLn "Problem 1"
--     print $ myLast [1, 2, 3, 4] == 4
--     print $ myLast ['x','y','z'] == 'z'


{-
 - Problem 2: Find the last but one element of a list.
 -}
myButLast :: [a] -> a
myButLast [x] = error "only one element in list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast' = last . init

-- -- test cases:
-- main = do
--     putStrLn "Problem 2"
--     print $ myButLast [1,2,3,4] == 3
--     print $ myButLast ['a'..'z'] == 'y'


{-
 - Problem 3: Find the K'th element of a list. The fst element in the list is number 1.
 -}
elementAt :: [a] -> Int -> a
elementAt xs num = xs !! (num - 1)

-- -- test cases:
-- main = do
--     putStrLn "Problem 3"
--     print $ elementAt [1,2,3] 2 == 2
--     print $ elementAt "haskell" 5 == 'e'


{-
 - Problem 4: Find the number of elements of a list.
 -}
myLength :: [a] -> Int
myLength xs = sum [1 | _ <- xs]

myLength' :: [a] -> Int
myLength' = sum . map (\_ -> 1)
-- myLength' xs = sum . map (\_ -> 1) xs

myLength'' :: [a] -> Int
myLength'' [] =  0
myLength'' (_:xs) =  1 + myLength'' xs

-- -- test cases:
-- main = do
--     putStrLn "Problem 4"
--     print $ myLength [123, 456, 789] == 3
--     print $ myLength "Hello, world!" == 13


{-
 - Problem 5: Reverse a list.
 -}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]  -- naive way LOL

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []
-- myReverse' = foldl (\acc x -> x : acc) []

-- -- test cases:
-- main = do
--     putStrLn "Problem 5"
--     print $ myReverse' "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"
--     print $ myReverse' [1,2,3,4] == [4,3,2,1]


{-
 - Problem 6: Find out whether a list is a palindrome.
 - A palindrome can be read forward or backward; e.g. (x a m a x).
 -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = if (head xs == last xs) then
                      isPalindrome $ (tail . init) xs
                  else False
-- isPalindrome xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

isPalindrome'' xs = xs == (reverse xs)  -- more efficient or not? memeory/intermediate var/speed

-- Here's one using foldr and zipWith.
palindrome''' :: Eq a => [a] -> Bool
palindrome''' xs = foldr (&&) True $ zipWith (==) xs (reverse xs)
palindrome'''' xs = and $ zipWith (==) xs (reverse xs) -- same, but easier

-- -- test cases:
-- main = do
--     putStrLn "Problem 6"
--     print $ isPalindrome [1,2,3] == False
--     print $ isPalindrome "madamimadam" == True
--     print $ isPalindrome [1,2,4,8,16,8,4,2,1] == True


{-
 - Problem 7 (**) Flatten a nested list structure.
 - Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
 - We have to define a new data type, because lists in Haskell are homogeneous.
 -}
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- test cases:
main = do
    putStrLn "Problem 7"
    print $ flatten (Elem 5) == [5]
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1, 2, 3, 4, 5]
    -- print $ flatten (List []) == []  -- FIXME: error?


{-
 - Problem 8 (**) Eliminate consecutive duplicates of list elements.
 - If a list contains repeated elements they should be replaced with a single copy of the element.
 - The order of the elements should not be changed.
 -}
compress :: Eq a => [a] -> [a]
compress xs = foldr (\x acc -> if x == head acc then acc else x:acc) [last xs] (init xs)
-- this one is not so efficient, because it pushes the whole input onto the "stack" before doing anything else.
-- why?

-- Note that GHC erases the Maybes, producing efficient code.
compress' :: Eq a => [a] -> [a]
compress' xs = foldr skipDups (const []) xs Nothing
    where skipDups x acc a@(Just q) | (x == q) = acc a
          skipDups x acc _ = x : acc (Just x)

compress'' :: Eq a => [a] -> [a]
compress'' = map head . group

-- -- test cases:
-- main = do
--     putStrLn "Problem 8"
--     print $ compress' "aaaabccaadeeee" == "abcade"


{-
 - Problem 9 (**) Pack consecutive duplicates of list elements into sublists.
 - If a list contains repeated elements they should be placed in separate sublists.
 -}
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = let (first, rest) = span (== head xs) xs in first : pack rest

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' xs = takeWhile (== head xs) xs : pack (dropWhile (== head xs) xs)

-- -- test cases:
-- main = do
--     putStrLn "Problem 9"
--     print $ pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa", "b", "cc", "aa", "d", "eeee"]


{-
 - Problem 10 (*) Run-length encoding of a list.
 - Use the result of problem P09 to implement the so-called run-length encoding data compression method.
 - Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
 -}
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack xs  -- pack = group

-- -- test cases:
-- main = do
--     putStrLn "Problem 10"
--     print $ encode "aaaabccaadeeee" == [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'),(1, 'd'), (4, 'e')]
