{-
    Some Higher Order Functions

    author  Matthew Williams
    version 1.0
    since   2019-06-12
-} 

import Data.Char

-- Takes a function and another argument and applies the function
-- three times to the argument.
thrice :: (a -> a) -> a -> a
thrice f x = 
    f (f (f x))

-- Removes spaces from both the beginning and the end of a string.
trimSp :: String -> String
trimSp s = 
    dropWhile isSpace (reverse (dropWhile isSpace (reverse s)))

-- Finds the successive differences between elements of a list.
-- diff between elements 1 and 2, 2 and 3, etc.
diffs :: [Int] -> [Int]
diffs xs = 
    zipWith (-) xs (tail xs)

-- Returns a list of all the 0-based indices of the locations of
-- x in ys
locations :: Eq a => a -> [a] -> [Int]
locations x ys = 
    map fst (filter secEqual (zip [0..] ys))
    where
        secEqual (m, n) = n == x

-- Returns a tubple where the first element is the longest prefix
-- of xs of elements that satisfy f, the second element is the 
-- remainder of the list.
splitter :: (a -> Bool) -> [a] -> ([a], [a])
splitter f xs = 
    ((takeWhile f xs),(dropWhile f xs))

splitterRec :: (a -> Bool) -> [a] -> ([a], [a])
splitterRec _ [] = ([],[])
splitterRec f (x:xs) = 
    if f x 
        then (x:(first), second)
        else ([], xs)
    where
        (first, second) = splitterRec f xs


{-

foldr works from the right and works its way back to the left
    takes a function, a base value and a list
    our base accumulator value is 0 and each time we apply foldr
        to a list we get an element and a base value
        we wish to ignore the value of the current element
        and just add one to our accumulator, we want to add 
        directly to the accumulator value and so we get a function
        that we apply to foldr which takes the accumulator value and then
        adds one to it

-}

-- Length function defined in terms of foldr
myLength xs = foldr f 0 xs
    where f x r = 1 + r

-- Map defined in terms of foldr
myMap f xs = foldr g [] xs
    where g x r = f x : r

-- Filter defined in terms of foldr
myFilter p xs = foldr g [] xs
    where g x r = if p x then x:r else r