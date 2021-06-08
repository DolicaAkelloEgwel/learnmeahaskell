-- Exercise 1.9
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- Example 1.18
minInt :: [Int] -> Int
minInt [] = error "empty list"
minInt [x] = x
minInt (x:xs) = min x (minInt xs)

-- Exercise 1.10
removeFirst _ []                 = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys

-- Example 1.11
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFirst m xs)) where m = minInt xs

-- Exercise 1.12
average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

-- Exercise 1.13
count :: Char -> String -> Int
count _ []                 = 0
count x (y:ys) | x == y    = 1 + count x ys
               | otherwise = count x ys

-- Exercise 1.14
repeatChar :: Int -> Char -> String
repeatChar 1 x = [x]
repeatChar n x = x : repeatChar (n - 1) x

traverseString :: Int -> String -> String
traverseString n [x] = repeatChar n x
traverseString n (x:xs) = repeatChar n x ++ traverseString (n + 1) xs 

blowup :: String -> String
blowup x = traverseString 1 x

-- Exercise 1.15
-- function that sorts a string in alphabetical order
sortString :: String -> String
sortString [] = []
sortString xs = min_letter : sortString (removeFirst min_letter xs) where min_letter = minimum xs

sortStrings :: [String] -> [String]
sortStrings [] = []
sortStrings xs = min_word : sortStrings (removeFirst min_word xs) where min_word = minimum xs

-- Example 1.16
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

-- Exercise 1.17
-- Check if str1 is a substring of str2
substring :: String -> String -> Bool
substring xs [] = False
substring xs (y:ys) = prefix xs (y:ys) || substring xs ys

factors :: Integer -> [Integer]
factors n | n < 1     = error "Argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n
