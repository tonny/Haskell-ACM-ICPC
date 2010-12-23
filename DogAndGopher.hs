{-
  Author  : to_On1
  Email   : antonio.mq@gmail.com
  Problem : Dog and Gopher {http://uva.onlinejudge.org/index.php?option=onlinejudge&page=show_problem&problem=1251}
  Instruction :
  Compile and execute with next command
  > ghc --make DogAndGopher
  > ./DogAndGopher < dogAndGopher.in
-}
main::IO()
main = mapM_ putStrLn . dog . lines =<< getContents

dog :: [String] -> [String]
dog [] = []
dog (x:xs) = let input   = words x
                 huecos  = if input /= [] 
                           then toEntero $ input !! 0
                           else 0
                 puntos  = map toFloat input
                 ardilla = (puntos !! 1, puntos !! 2)
                 perro   = (puntos !! 3, puntos !! 4)
                 posHuecos = map (map toFloat) $ map words $ take huecos xs
                 resto = drop huecos xs
             in
             (calcular perro ardilla posHuecos False) : dog resto
 where
 calcular :: (Float,Float) -> (Float,Float) -> [[Float]] -> Bool -> String
 calcular _ _ [] eat = if eat then "The gopher cannot escape." else ""
 calcular perro ardilla (x:xs) eat = let distArdilla = opPuntos ardilla (x !! 0 , x !! 1)
                                         distPerro   = opPuntos perro (x !!0 , x !! 1)
                                     in if (4 * distArdilla) <= distPerro
                                        then "The gopher can escape through the hole at ("++
                                             (show $ x !! 0)++ ","++(show $ x !! 1)++")."
                                        else calcular perro ardilla xs True 

 opPuntos :: (Float,Float) -> (Float,Float) -> Float
 opPuntos p1 p2 = ((fst p1 - fst p2)^2) + ((snd p1 - snd p2)^2)

toEntero :: String -> Int
toEntero = read 

toFloat :: String -> Float
tioFloat = read
