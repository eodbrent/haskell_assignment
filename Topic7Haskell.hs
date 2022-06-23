len :: [a] -> Float
len [] = 0
len (_:xs) = 1 + len xs

max' :: Ord a => [a] -> a
max' [x] = x
max' (x:xs)
 | x > maxTail = x
 | otherwise = maxTail
 where maxTail = max' xs

min' :: Ord a => [a] -> a
min' [x] = x
min' (x:xs)
 | x < minTail = x
 | otherwise = minTail
 where minTail = min' xs

sumInts :: [Float] -> Float
sumInts [] = 0
sumInts(x:xs) = x + sumInts xs

square :: Float -> Float
square x = x * x

displayStats :: [Float] -> IO()
displayStats ints = do
 let intsLength = len ints
 let avg = sumInts ints / intsLength
 let absfrommean = map (\x -> abs (x - avg)) ints
 let meanAbsDev = sumInts absfrommean / intsLength
 let variance = sumInts (map (square) absfrommean) / intsLength
 putStrLn ("List of values: " ++ show ints)
 putStrLn ("Minimum:                 " ++ show (min' ints))
 putStrLn ("Maximum:                 " ++ show (max' ints))
 putStrLn ("Average:                 " ++ show avg)
 putStrLn ("Mean Absolute Deviation: " ++ show meanAbsDev)
 putStrLn ("Standard Deviation:      " ++ show (sqrt variance))
 putStrLn ("Variance:                " ++ show variance)
