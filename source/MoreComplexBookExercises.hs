{-
    Some exercises based on those in An Introduction to Functional 
    Programming Systems using Haskell - AJT Davie 
    and from The Craft of Functional Programming - Simon Thompson

    author  Matthew Williams
    version 1.0
    since   2019-06-13
-} 

import Data.Char

-- Factorial Function
myFactorial :: Int -> Int
myFactorial 0 = 1
myFactorial n = n * myFactorial (n-1)

choose :: Int -> Int -> Int
choose m n = (myFactorial(m)) `div` (myFactorial(n) * myFactorial(m-n))

-- Approximation to the Square Root function using the Newton Raphson method
improveGuess :: Double -> Double -> Double
improveGuess x a = 0.5 * (x + (a/x))

approxSqrt :: Double -> Double -> Double
approxSqrt x a =
    if (abs(x - (improveGuess x a)) < precision) 
        then x
        else approxSqrt (improveGuess x a) a
    where 
        precision = 0.000005

-- Copies a list of numbers except negative numbers which are replaced by zero
naturalise :: [Int] -> [Int]
naturalise [] = []
naturalise (x:xs) =
    if x > 0
        then x : (naturalise xs)
        else 0 : (naturalise xs)

naturaliseMap :: [Int] -> [Int]
naturaliseMap xs = 
    map f xs
    where
        f x = 
            if x > 0
                then x
                else 0

-- Applies a function f to integers m to n inclusive
--  ie returns the list [f(m), f(m+1), ..., f(n)]
for :: Int -> Int -> (Int -> Int) -> [Int]
for m n f =
    if m < n
        then map f [m..n]
        else reverse (map f [n..m])

-- Selects the sub-list of a list starting at the m’th 
-- element and ending at the n’th
at :: Int -> [a] -> a
at n [] = error "Index not found"                      -- error - crash
at 0 (x:xs) = x
at n (x:xs) = at (n-1) xs

getSublist :: Int -> Int -> [Int] -> [Int]
getSublist m n xs =
    for m n myFunct
    where
        myFunct k = at k xs

-- Improved version of at function above. Also indexes from 
-- the right hand end of list if given negative argument.
atImproved :: Int -> [a] -> a
atImproved n [] = error "Index not found - cannot operate on an empty list"
atImproved 0 (x:xs) = x
atImproved (-1) (xs) = head (reverse xs)
atImproved n (x:xs) = 
    if n > 0
        then atImproved (n-1) xs
        else atImproved (n+1) (reverse ((drop 1 (reverse (x:xs))))) -- negative, we want to start at the right hand end 

-- Finds last element of a list using recursion
final :: [a] -> a
final [] = error "Cannot operate on an empty list"
final xs = 
    finalHelper (length xs) xs
    where
        finalHelper :: Int -> [a] -> a
        finalHelper n [] = error "Cannot operate on an empty list"
        finalHelper l (x:xs) = 
            if (l > 1) 
                then finalHelper (l - 1) xs
                else x -- i.e. when we reach list length of 1 we are at the last elem.

-- Removes the nth item from a list
-- eg   removeNthItem 2 [1..10]
removeNthItem :: Int -> [a] -> [a]
removeNthItem n [] = []
removeNthItem 1 (x:xs) = xs
removeNthItem n (x:xs) = 
    x:(removeNthItem (n-1) xs)

-- Justifies text in a given column width by inserting spaces as necessary.
-- User can choose justification using arguments l, c, or r.
-- eg   justify 'c' 2 "hello" 
--      justify 'l' 2 "hello"
--      justify 'r' 2 "hello"
justify :: Char -> Int -> String -> String
justify pos 0 str =
    str
justify pos col str = 
    if (pos == 'c') -- centre
        then justify pos (col - 1) ("    " ++ str ++ "    ")
    else if (pos == 'r') -- right
        then justify pos (col - 1) ("    " ++ str)
    else --left
        justify pos (col - 1) (str ++ "    ")

-- Return average of three integers
-- eg   averageThree 1 2 3
--      howManyAboveAverage 1 2 3
averageThree :: Int -> Int -> Int -> Float
averageThree a b c = fromIntegral(a + b + c) / 3

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage a b c = length (filter (> average) [fromIntegral(a),fromIntegral(b),fromIntegral(c)])
    where 
        average = averageThree a b c

-- Converts all lower case letters in a string into cacpitals
-- also removing all non-letters from the list.
-- eg   capitalise "fwfEEWEW@£4334!efjoj"
capitalise :: String -> String
capitalise str =
    capNoAlpha (length str) str
    where
        capNoAlpha :: Int -> String -> String
        capNoAlpha 0 str = str
        capNoAlpha n (char:str) =
            if isAlpha(char)
                then (toUpper(char)):(capNoAlpha (n - 1) str)
                else capNoAlpha (n - 1) str

-- Returns the list of divisors of a positive integer
-- and the empty list for other inputs
-- eg   divisors 12
divisors :: Int -> [Int]
divisors num =
    if num < 1 
        then [] 
        else [x | x <- [1..num], num `mod` x == 0]

-- Returns the number of times that n occurs in the list xs.
-- elemNumA uses recursion. elemNumB uses a list comprehension
-- elemNumC uses functions from the Prelude.
-- eg   elemNumA 2 [1,2,3,43,34,3145,2,2,3,4,1]
--      elemNumB 2 [1,2,3,43,34,3145,2,2,3,4,1]
--      elemNumC 10 [1,2,32,4325,52,2534,10,10,10,322,21,10]
elemNumA :: Int -> [Int] -> Int
elemNumA n xs =
    elemNumAHelper 0 n xs
    where
        elemNumAHelper c n [] = c
        elemNumAHelper c n (x:xs) =
            if (x == n)
                then elemNumAHelper (c + 1) n xs
                else elemNumAHelper c n xs

elemNumB :: Int -> [Int] -> Int
elemNumB n xs = length [1 | x <- xs, x == n]

elemNumC :: Int -> [Int] -> Int
elemNumC n xs = length (filter (== n) xs)