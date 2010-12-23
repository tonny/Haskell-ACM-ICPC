{-================================================================================
 Author  : to_On1
 Email   : antonio.mq@gmail.com
 Porblem : Searching Quickly
 http://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&category=3&page=show_problem&problem=59
=================================================================================-}
import Data.List
import Char

main :: IO()
main = mapM_ putStrLn . imprimir . lines =<< getContents

imprimir :: [String] -> [String]
imprimir xs = let (ignorar,titles) = span (/="::") $ map ( map toLower ) xs 
                  clean = map words ( tail titles )
                  sol   = sort $ concat $ map (filter (\ x -> not $ elem x ignorar )) clean
                  grupo = groupBy (==) sol
                  res   = concat $  map ( \ (x:xs) -> solution x clean ) grupo
              in  map (\ x -> foldr (\ a b -> a ++ " " ++ b) ""  x) res
     where 
     solution :: String -> [[String]] -> [[String]]
     solution _ [] = []
     solution x (y:ys) = pintar 0 0 x y y ++ solution x ys
              
     pintar :: Int -> Int -> String -> [String]-> [String] -> [[String]]
     pintar _ _ _ _ [] = []
     pintar pos act cad aux (x:xs) | cad == x && pos == act = (paint pos 0 cad aux) :
                                                              (pintar (pos+1) (act+1) cad aux xs)
                                   | cad == x  = pintar pos (act+1) cad aux xs
                                   | otherwise = pintar pos act cad aux xs

     paint :: Int -> Int -> String -> [String] -> [String]
     paint _ _ _ [] = []
     paint pos act cad (x:xs) | cad == x && pos == act = (map toUpper x):xs
                              | cad == x = x : paint pos (act+1) cad xs
                              | otherwise = x : paint pos act cad xs 

