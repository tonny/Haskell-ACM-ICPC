module Main where

{--
Problem : Roman Calculator
--}

main :: IO ()
main = calculator . lines =<< getContents

calculator :: [String] -> IO ()
calculator (_:xs) = mapM_ (putStrLn . result . words) xs
 
result :: [String] -> String
result (x:sop:y:_) = toRoman ((fromRoman x) `op` (fromRoman y))
 where op = maybe (\x y -> 0) id (lookup sop ops)

ops    = [("+",(+)),("-",(-)),("*",(*)),(":",div),("%",mod)]

fromRoman :: String -> Int
fromRoman [] = 0
fromRoman (l:ls) = number + fromRoman rest
 where (number,rest) = convertToNumber l ls

convertToNumber l ls
 | l == 'C' = case ls of
                ('M':ys) -> (900, ys)
                ('D':ys) -> (400, ys)
                _        -> (100, ls)
 | l == 'X' = case ls of
                ('C':ys) -> (90 , ys)
                ('L':ys) -> (40 , ys)
                _        -> (10 , ls)
 | l == 'I' = case ls of
                ('X':ys) -> (9  , ys)
                ('V':ys) -> (4  , ys)
                _        -> (1  , ls)
 | l == 'M' = (1000, ls)
 | l == 'D' = (500 , ls)
 | l == 'L' = (50  , ls)
 | l == 'V' = (5   , ls)

toRoman :: Int -> String
toRoman n | n > 0     = toRoman' n
         | n < 0     = '-' : (toRoman' (abs n))
         | otherwise = "ERROR"

toRoman' 0 = ""
toRoman' num1000 = letters1000 ++ letters100 ++ letters10 ++ letters1
 where letters1000   = take (num1000 `div` 1000) (repeat  'M')

       num100        = num1000 `mod` 1000
       letters100    = getUnit (num100 `div` 100) (units 'C' 'D' 'M')

       num10         = num100  `mod` 100
       letters10     = getUnit (num10 `div` 10)   (units 'X' 'L' 'C')

       num1          = num10   `mod` 10
       letters1      = getUnit num1               (units 'I' 'V' 'X')

getUnit 0 _  = ""
getUnit n us = us !! (n-1)
units u h t = [u1, u2, u3, u:h1, h1, h:u1, h:u2, h:u3, u:t1]
 where u1 = u:[]
       u2 = u:u1
       u3 = u:u2
       h1 = h:[]
       t1 = t:[]
