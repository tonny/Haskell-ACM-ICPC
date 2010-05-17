{--
 Author      : to_On1 
 Porblem     : Small factorials 
 url Problem : https://www.spoj.pl/problems/FCTRL2/
--}

main :: IO()
main = mapM_ putStrLn . factorial . lines =<< getContents 

factorial :: [String] -> [String]
factorial (x:xs) = map (\ x-> show ( foldr (*) 1 [1..(toEntero x)] )) xs

toEntero :: String -> Integer
toEntero = read

