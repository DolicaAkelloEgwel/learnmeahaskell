maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

minInt :: [Int] -> Int
minInt [] = error "empty list"
minInt [x] = x
minInt (x:xs) = min x (minInt xs)

removeFirst _ []                 = []
removeFirst x (y:ys) | x == y    = ys
                     | otherwise = y : removeFirst x ys

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFirst m xs)) where m = minInt xs

average :: [Int] -> Rational
average [] = error "empty list"
average xs = toRational (sum xs) / toRational (length xs)

count :: Char -> String -> Int
count _ []                 = 0
count x (y:ys) | x == y    = 1 + count x ys
               | otherwise = count x ys

repeatChar :: Int -> Char -> String
repeatChar 1 x = [x]
repeatChar n x = [x] ++ repeatChar (n - 1) x

traverseString :: Int -> String -> String
traverseString n [x] = repeatChar n x
traverseString n (x:xs) = repeatChar n x ++ traverseString (n + 1) xs 

blowup :: String -> String
blowup x = traverseString 1 x
