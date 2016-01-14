-- Haskell Programs

-- Custom Head
head' :: [a] -> a
head' [] = error "Can't do the head operation on an empty list!"
head' (x:_) = x

-- Custom Tail
tail' :: [a] -> [a]
tail' [] = error "Can't do the tail operation on an empty list!"
tail' (_:x) = x

-- Custom Length
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:x) = 1 + length' x

-- Custom Sum
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Custom BMI Index Calculator
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Overweight"
    | otherwise   = "!!!!!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.0, 25.0, 30.0)

-- Custom pair-wise BMI Index calculator
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi' w h | (w, h) <- xs]  
    where bmi' weight height = weight / height ^ 2

-- Find the last element of a list
mylast :: [a] -> a
mylast [] = error "Cannot find the last element of an empty list."
mylast [x] = x
mylast (_:xs) = mylast xs

-- Find the last element of a list v2
mylastv2 :: [a] -> a
mylastv2 = head . reverse

-- Find the last element of a list v3
mylastv3 :: [a] -> a
mylastv3 x = x !! (length x - 1)

-- Find last but one element of a list
mylastbut1 :: [a] -> a
mylastbut1 [] = error "Cannot find the last but one element of an empty list."
mylastbut1 [x] = error "Cannot find the last but one element of a single element list."
mylastbut1 x = x !! (length x - 2)

-- Find the k-th element of a list
elementAt :: [a] -> Int -> a
elementAt [] k = error "Cannot find the k-th element of an empty list"
elementAt x k = if k <= length x then x !! (k-1) else error "k < the length of the list"

-- IsPalindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = (x == (reverse x))

-- Count the elements in a list
count :: Eq a => a -> [a] -> Int
count n [] = 0
count n (x:xs) | n == x = 1 + count n xs
    | otherwise = count n xs

counting x = [(y, count y x) | y <- x]

-- Count the elements in a list - frequency
freq :: Eq a => [a] -> [(a, Int)]
freq [] = []
freq (z:zs) = [(z, count z (z:zs))] ++ freq (filter (/= z) zs)

-- Extract the unique elements
rmdups :: Eq a => [a] -> [a]
rmdups [ ] = [ ]
rmdups (x:xs) = x : rmdups (filter(/= x) xs)

-- max'
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

-- min'
min' :: (Ord a) => a -> a -> a
min' a b
    | a < b = a
    | otherwise = b

-- maxl
-- maxl :: (Ord a) => [a] -> a
-- maxl [] = []
-- maxl (x:xs) = max' x (maxl xs)

-- group
-- group' x = group x

-- clone
clone :: a -> Int -> [a]
clone x n = if n == 0 then [] else x:(clone x (n-1))

-- clone
clone' x 0  = []
clone' x n = x:(clone' x (n-1))
