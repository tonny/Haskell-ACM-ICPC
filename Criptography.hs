{-
 Author  : to_On1
 Porblem : Criptography
 Version : 100711
-}

main :: IO()
main = mapM_ putStrLn . decode . lines =<< getContents

decode :: [String] -> [String]
decode (x:xs) = des xs
              where 
              des :: [String] -> [String]
              des [] = [] 
              des (x:y:z:zs) = let kay = concat $ words z
                               in (desifrar y kay kay): des zs

desifrar :: String -> String -> String -> String
desifrar [] _ _ = ""
desifrar (x:xs) (y:ys) zs | x == ' ' = ' ' : desifrar xs (y:ys) zs
                          | es x = let num1  = snd $ head $ filter (\ z -> fst z == x ) letras
                                       num2  = snd $ head $ filter (\ z -> fst z == y ) letras
                                       resta = num1 - (num2-1)
                                       res   = if ys == [] then zs else ys
                                   in if resta <= 0 then 
                                      (fst $ head $ filter (\w-> snd w == 26+resta)letras):desifrar xs res zs
                                      else
                                      (fst $ head $ filter (\w-> snd w == resta) letras) : desifrar xs res zs
                           | otherwise = let res =  if ys == [] then zs else ys
                                           in x : desifrar xs res zs 
                          where
                          es e = elem e ['a'..'z']
--letras :: [(Char,Int)]
letras = [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6),('g',7),('h',8),('i',9),('j',10),
          ('k',11),('l',12),('m',13),('n',14),('o',15),('p',16),('q',17),('r',18),('s',19),
          ('t',20),('u',21),('v',22),('w',23),('x',24),('y',25),('z',26)
         ]

