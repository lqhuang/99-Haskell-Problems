{-
 - Part 03
 - Lists again
 -}
import Data.List           (elemIndex)
import Control.Applicative ((<$>))
import System.Random
import Part02              (removeAt)

{-
 - Problem 21 Insert an element at a given position into a list.
 -}
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs idx = first ++ x : second
    where (first, second) = splitAt (idx - 1) xs

-- -- test cases:
-- main = do
--     putStrLn "Problem 21"
--     print $ insertAt 'X' "abcd" 2 == "aXbcd"


{-
 - Problem 22 Create a list containing all integers within a given range.
 -}
range :: Int -> Int -> Int -> [Int]
range start stop 1 = [start..stop]  -- Note that range list in haskell doesn't follow [,) rule.
range start stop step = [start, start + step .. real_stop]  -- Python-like range
    where real_stop = if (stop-start) `mod` step == 0 then stop - step else stop
-- TODO: how to implement/define optional arguments?

range' :: Int -> Int -> Int -> [Int]
range' start stop 1 = take (stop - start + 1)  $ iterate (+1) start
range' start stop step = take ((stop - start + step) `div` step) $ iterate (+ step) start

-- pred/succ and handle start > stop condition
range'' :: (Ord a, Enum a) => a -> a -> [a]
range'' a b | (a == b) = [a]
range'' a b = a:range'' ((if a < b then succ else pred) a) b

-- use scanl
range''' :: Int -> Int -> [Int]
range''' l r = scanl (+) l (replicate (l - r) 1)

-- Since there's already syntactic sugar for ranges, there's usually no reason to define a function like 'range' in Haskell.
-- In fact, the syntactic sugar is implemented using the `enumFromTo` function, which is exactly what 'range' should be.

-- -- test cases:
-- main = do
--     putStrLn "Problem 22"
--     -- print $ range 4 9 == [4, 5, 6, 7, 8, 9]
--     print $ range 4 9 1 == [4, 5, 6, 7, 8, 9]
--     print $ range 0 10 2 == [0, 2, 4, 6, 8]
--     print $ range 0 9 2 == [0, 2, 4, 6, 8]


{-
 - Problem 23 Extract a given number of randomly selected elements from a list.
 -}
{-
 - old version
 - `n < 0` is redundant, because `take` function would check `n` argument.

    rnd_select :: [a] -> Int -> [a]
    rnd_select [] _ = []
    rnd_select xs n
        | n < 0     = error "number of elements must be greater than zero."
        | otherwise = map (xs !!) $ take n $ randomRs (0, len-1) (mkStdGen 100)
            where len = length xs
 -}
rnd_select :: [a] -> Int -> [a]
rnd_select [] _ = []
rnd_select xs n = map (xs !!) $ take n $ randomRs (0, len-1) (mkStdGen 100)
    where len = length xs

-- Use applicative and global random generator and result in different type signatures.
rnd_select' :: [a] -> Int -> IO [a]
rnd_select' [] _ = return []
rnd_select' xs n = map (xs !!) <$> take n . randomRs (0, len-1) <$> getStdGen
        where len = length xs

-- test cases:
main = do
    putStrLn "Problem 23"
    print $ rnd_select "abcdefgh" 3
    print $ rnd_select "abcdefgh" 3 == "beg"
    rnd_select' "abcdefgh" 3 >>= putStrLn
    rnd_select' "abcdefgh" 3 >>= putStrLn
    -- Note: notice that run last command twice will get totally the same result.

{-
 - Problem 24 Lotto: Draw N different random numbers from the set 1..M.
 -}
diff_select :: Int -> Int -> [Int]
diff_select n m
    | n > m         = error "Error: number of list must be greater than number of selected."
    | otherwise     = rnd_select [1 .. m] n

-- -- test cases:
-- main = do
--     putStrLn "Problem 24"
--     print $ diff_select 6 49
--     print $ diff_select 6 49 == [23, 1, 17, 33, 21, 37]


{-
 - Problem 25 Generate a random permutation of the elements of a list.
 -}
rnd_permu :: [a] -> [a]
rnd_permu xs =

-- -- test cases:
-- main = do
--     putStrLn "Problem 25"
--     print $ rnd_permu "abcdef" == "badcef"


{-
 - Problem 26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
 - In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
    there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
 - For pure mathematicians, this result may be great.
 - But we want to really generate all the possibilities in a list.
 -}
-- combinations :: Int -> [a] -> [[a]]
-- combinations k xs |


-- -- test cases:
-- main = do
--     putStrLn "Problem 26"
--     print $ combinations 3 "abcdef" == ["abc", "abd", "abe", ...]



{-
 - Problem 27 Group the elements of a set into disjoint subsets.
 - a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
 - Write a function that generates all the possibilities and returns them in a list.
 - b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
 - Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
 - However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

 - You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
 -}

-- -- test cases:
-- main = do
--     putStrLn "Problem 27"
--     print $ group [2,3,4] ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"] == [[["aldo", "beat"], ["carla", "david", "evi"], ["flip", "gary", "hugo", "ida"]], ...]
-- -- (altogether 1260)
--     print $ group [2,2,5] ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"] == [[["aldo", "beat"], ["carla", "david"], ["evi", "flip", "gary", "hugo", "ida"]], ...]
-- -- (altogether 756)


{-
 - Problem 28 Sorting a list of lists according to length of sublists
 - a) We suppose that a list contains elements that are lists themselves.
 - The objective is to sort the elements of this list according to their length.
 - E.g. short lists first, longer lists later, or vice versa.
 -}



-- -- test cases:
-- main = do
--     putStrLn "Problem 28"

-- lsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] == ["o", "de", "de", "mn", "abc", "fgh", "ijkl"]


{-
 - (b) Again, we suppose that a list contains elements that are lists themselves.
 - But this time the objective is to sort the elements of this list according to their length frequency;
 - i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
 -}

-- -- test cases:
-- main = do
--     putStrLn "Problem 21"

-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] == ["ijkl", "o", "abc", "fgh", "de", "de", "mn"]
