{--

 Author : to_On1
 Problem : Roman Calculator
 url Problem : http://www.spoj.pl/SHORTEN/problems/ROMANCAL/

--}

main :: IO()
main = mapM_ putStrLn . calculator . lines =<< getContents 

calculator :: [String] -> [String]
calculator (x:xs) = map (\ y -> result (words y) ) xs

result :: [String] -> String
result (x:y:z:xs) = case y of
                     "+" -> convert ((aNum x roma) + (aNum z roma))
                     "-" -> let num = ((aNum x roma) - (aNum z roma))
                             in if num < 0 then "-"++convert (-1 * num) 
                                else convert num
                     "*" -> convert ((aNum x roma) * (aNum z roma))
                     ":" -> convert (div (aNum x roma) (aNum z roma))
                     "%" -> convert (mod (aNum x roma) (aNum z roma))


roma =[("M",1000),("CM",900),("D",500),("CD",400),("C",100),("XC",90),("L",50),("XL",40),
        ("X",10),("IX",9),("V",5),("IV",4),("I",1)]

aNum :: String -> [(String,Integer)] -> Integer
aNum "" _ = 0
aNum (x:xs) ys | length xs > 0 = if filter (\ z -> fst z == (x:(head xs):[])) ys == []
                                   then ( snd ( (filter (\ z -> fst z == x:[] ) ys) !! 0)) + aNum xs ys
                                   else  (snd ((filter (\ z -> fst z == (x:(head xs):[])) ys)!! 0)) + aNum (tail xs) ys
                | otherwise =  (snd (filter (\ z -> fst z == x:[]) ys !! 0)) + aNum xs ys

convert :: Integer -> String 
convert  num | num > 0 = roman 0 num
             | otherwise = "ERROR" 
           where 
             roman _ 0 = ""
             roman p n | n >= snd (roma !! p) = fst (roma !! p) ++ roman p (n - snd(roma!!p))
                       | otherwise = roman (p+1) n

