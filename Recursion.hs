{-
    Some Recursive Functions

    author  Matthew Williams
    version 1.0
    since   2019-06-11
-} 

-- Test if an element y is in a list xs
member :: Int -> [Int] -> Bool
member y [] = False
member y (x:xs) = 
    if y == x
        then True
        else member y xs

-- Takes a list and using recursion returns the list without its last element
beginning :: [a] -> [a]
beginning [] = []
beginning [x] = []
beginning (y:x:xs) = y:(beginning (x:xs))

-- Takes an integer n and list xs and using recursion returns the item at
-- position n in the list
at :: Int -> [a] -> a
at n [] = error "Index not found"                      -- error - crash
at 0 (x:xs) = x
at n (x:xs) = at (n-1) xs
    
-- Produces a list using recursion containing all the ints between arguments
-- i and j
interval :: Int -> Int -> [Int]
interval i j =
    if i == j
        then [i]
    else if i < j 
        then i:(interval (i+1) j)
    else -- i > j
        i:(interval (i-1) j)

-- Duplicates every item in a list
duplicate :: [a] -> [a]
duplicate [] = []
duplicate [x] = x:[x]
duplicate (x:xs) = x:x:(duplicate xs)

-- Takes an element e and a list xs and puts e between each element of xs
between :: a -> [a] -> [a]
between e [] = []
between e [x] = [x]
between e (x:xs) = x:e:(between e xs)

-- Takes a list xs and using recursion returns all initial segments of xs, 
-- shortest first
beginnings :: [a] -> [[a]]
beginnings [] = [[]]
beginnings (x:xs) = 
    []:(map x (beginnings xs))