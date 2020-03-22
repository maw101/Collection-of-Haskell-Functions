{-
    Language Basics - a small collection of functions utilising basics of Haskell.
    Some functions are commented out as they are improved upon later in the file.

    author  Matthew Williams
    version 1.0
    since   2019-06-10
-} 

-- Greet a person given their name and their language as arguments
{-
greet :: String -> Char -> String
greet name lang = 
    if lang == 'e'
        then "Hello " ++ name
        else if lang == 'f' then "Bonjour " ++ name
        else "Hi " ++ name
-}
        
-- Calculate the sum of the squares of both the sum and the difference
-- of two integers
{-
sumDiff :: Int -> Int -> Int
sumDiff num1 num2 = 
    let
        s = num1 + num2
        d = num1 - num2
    in
    (s ^ 2) + (d ^ 2)
-}

-- Calculate the sum of the squares of both the sum and the difference
-- of two integers
sumDiff :: Int -> Int -> Int
sumDiff num1 num2 = 
    (s ^ 2) + (d ^ 2)
    where
        s = num1 + num2
        d = num1 - num2
        
-- Calculates the Circumference and Area of a circle given a radius.
-- Returns a pair representing these.
{-
circumAndArea :: Float -> (Float, Float)
circumAndArea r = 
    (circumference, area)
    where 
        circumference = (2 * pi * r)
        area = (pi * r^2)
-}

-- Calculates the Circumference and Area of a circle given a radius.
-- Returns a pair representing these. 
circumAndArea :: Float -> (Float, Float)
circumAndArea r = 
    let 
        circumference = (2 * pi * r)
        area = (pi * r^2)
    in
        (circumference, area)        
        
-- Greet a person given their name and their language as arguments
greet :: String -> Char -> String
greet name lang =
    case lang of
        'e' -> "Hello " ++ name
        'f' -> "Bonjour " ++ name
        _ -> "Hi " ++ name
        
-- Takes an age as an integer argument and produces a birthday message
birthdayMessage :: Int -> String
birthdayMessage age 
    | age < 19  = "Happy Birthday, you're " ++ (show age) ++ "!"
    | age > 99 = "Happy Birthday from the Queen"
    | otherwise = "Happy birthday, another year older"
    
-- Determines the largest of two integer arguments
{-
largest :: Int -> Int -> Int
largest x y =
    if x > y 
        then x
        else y
-}

-- Determines the largest of two integer arguments
largest :: Int -> Int -> Int
largest x y
    | x > y = x
    | otherwise = y

-- Takes an integer argument (1 to 7 inclusive) representing the day of 
-- the week and gives the string equivalent.
{-
dayOfTheWeek :: Int -> String
dayOfTheWeek numericDay =
    if numericDay == 1 then "Monday"
    else if numericDay == 2 then "Tuesday"
    else if numericDay == 3 then "Wednesday"
    else if numericDay == 4 then "Thursday"
    else if numericDay == 5 then "Friday"
    else if numericDay == 6 then "Saturday"
    else if numericDay == 7 then "Sunday"
    else "Enter between 1 and 7 inclusive"
-}

-- Takes an integer argument (1 to 7 inclusive) representing the day of 
-- the week and gives the string equivalent.  
dayOfTheWeek :: Int -> String
dayOfTheWeek numericDay =
    case numericDay of
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        7 -> "Sunday"
        _ -> "Enter between 1 and 7 inclusive"
    
-- Takes a grade argument and gives a range pair for this grade
type Grade = Char
type Range = (Int, Int)

rangeForGrade :: Grade -> Range
rangeForGrade grade = 
    case grade of 
        'A' -> (70, 100)
        'B' -> (60, 69)
        'C' -> (50, 59)
        'D' -> (40, 49)
        'E' -> (35, 39)
        'F' -> (0, 34)
        
-- Determines the largest of three integer arguments
{-
largestOfThree :: Int -> Int -> Int -> Int
largestOfThree x y z = 
    if ((x > y) && (x > z)) then x
    else if ((y > x) && (y > z)) then y
    else z
-}    

-- Determines the largest of three integer arguments 
largestOfThree :: Int -> Int -> Int -> Int
largestOfThree x y z
    | ((x > y) && (x > z)) = x
    | ((y > x) && (y > z)) = y
    | otherwise = z  