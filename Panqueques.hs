{-=============================================================================
 Author  : to_On1
 Email   : antonio.mq@gmail.com
 Problem : Panqueques
=============================================================================-}

main::IO()
main= mapM_ putStrLn . panqueque . lines =<< getContents

panqueque :: [String] -> [String]
panqueque [] = []
panqueque (x:xs) = x : ordenar (reverse x) "" 0 : panqueque xs
     where
     ordenar :: [Char] -> String -> Int -> String
     ordenar [] _ _ = []
     ordenar (x:xs) res pos = if isOrdered (x:xs) 
                              then res++"0"
                              else let (mayor,p,t) = foldl major ('0',0,0) (x:xs)  -- "-1"
                                       izq = takeWhile (/= mayor) (x:xs)
                                       der = reverse $ dropWhile (/= mayor) (x:xs)
                                       vol = izq ++ der
                                   in if p == t 
                                      then ordenar (tail $ reverse vol) (res++show (pos+1)) (pos+1) 
                                      else if p == 1
                                           then ordenar (tail vol) (res++"1") (pos+1)
                                           else ordenar vol (res++show (p+pos)) 0
                                                         
isOrdered :: [Char] -> Bool
isOrdered (x:xs) | xs /= []  = if x >= head xs then isOrdered xs
                                               else False
                 | otherwise = True

major :: (Char,Int,Int) -> Char -> (Char,Int,Int)
major (x,y,z) c = if c > x then (c,(z+1),(z+1))
                            else (x,y,(z+1))

