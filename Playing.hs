{-
  Author  : to_On1
  email   : antonio.mq@gmail
  problem : Playing With Wheels http://uva.onlinejudge.org/external/100/10067.html
  Instruction :
  Compile and execute with the next commmand
  > ghc -- make Playing.hs
  > ./Playing < playing.in
-}

main::IO()
main= mapM_ putStrLn . playing . tail . lines =<< getContents

playing :: [String] -> [String]
playing [] = []
playing (x:y:z:xs) = let input      = map toEntero $ words x
                         ouput      = map toEntero $ words y
                         restrict   = map words $ take (toEntero z) xs
                         restricted = map (map toEntero) restrict
                         resto      = drop (toEntero z) xs
                     in
                     show (rotar input ouput restricted 0) : playing resto

-- Combierte a Entero
toEntero :: String -> Int
toEntero = read

--Funcio que rota en centido Horario y Antihorario
rotar :: [Int] -> [Int] -> [[Int]] -> Int -> Int
rotar [] [] _ _ = 0
rotar (x:xs) (y:ys) res pos = case lookup x lista of
                              Just n -> case lookup y lista of
                                        Just m -> let col = map (\ c -> c !! pos) res
                                                      may = if n == 9 then 0 else (n + 1)
                                                      men = if n == 0 then 9 else (n - 1)
                                                      hor = horario may m col 1
                                                      ant = antiHor men m col 1
                                                  in
                                                    if fst hor && fst ant
                                                    then (min (snd hor) (snd ant)) + rotar xs ys res (pos+1)
                                                    else if fst hor
                                                         then (snd hor) + rotar xs ys res (pos+1)
                                                         else if fst ant 
                                                              then (snd ant) + rotar xs ys res (pos+1)
                                                              else -1
                                        _      -> -1
                              _      -> -1

-- Movimiento Horario
horario :: Int -> Int -> [Int] -> Int -> (Bool,Int)
horario x y col pos = case x == y of 
                      True  -> (True,pos)
                      False -> case x == 9 of
                               True  -> if elem x col
                                        then (False,-1)
                                        else horario 0 y col (pos+1)
                               False -> if elem x col 
                                        then (False,-1)
                                        else horario (x+1) y col (pos+1)
--Movimiento Anti Horario                                             
antiHor :: Int -> Int -> [Int] -> Int -> (Bool,Int)
antiHor x y col pos = case x == y of
                      True  -> (True,pos)
                      False -> case x == 0 of
                               True  -> if elem x col
                                        then (False,-1)
                                        else antiHor 9 y col (pos+1)
                               False -> if elem x col
                                        then (False,-1)
                                        else antiHor (x-1) y col (pos+1) 

lista = [(0,5),(1,6),(2,7),(3,8),(4,9),(5,0),(6,1),(7,2),(8,3),(9,4)]
