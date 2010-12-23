{-
  Author  : to_On1
  Email   : antonio.mq@gmail.com
  Problem : Bicoloring {http://uva.onlinejudge.org/external/100/10004.html}
-}
main :: IO()
main = mapM_ putStrLn . bicolor . init . lines =<< getContents

bicolor :: [String] -> [String]
bicolor [] = []
bicolor (x:y:xs) = let grafo = concat $ map words (take (toEntero y) xs)
                       resto = drop (toEntero y) xs
                   in if isBicolor grafo [("-1",False)] 0
                      then "NOT BICOLARABLE":bicolor resto
                      else "BICOLARABLE":bicolor resto
            where 
            isBicolor :: [String] -> [(String,Bool)] -> Int -> Bool
            isBicolor [] _ _ = True
            isBicolor (x:xs) lista col | col == 0 = case lookup x lista of
                                                    (Just y) -> if y
                                                                then isBicolor xs lista (col+1) 
                                                                else False

                                                    _        -> let insert = (y,True):lista
                                                                in isBicolor xs insert (col+1)

                                       | otherwise = case lookup x lista of
                                                     (Just y) -> if y 
                                                                 then False
                                                                 else isBicolor xs lista (col-1)
                                                     _        -> let insert = (y,False):lista
                                                                 in isBicolor xs insert (col-1)

toEntero:: String -> Int
toEntero = read

