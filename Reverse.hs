{-- 
 Author : to_On1
 Problem : Reverse order
 url Problem : https://www.spoj.pl/SHORTEN/problems/REVORDER/
--}
main :: IO ()
main =  mapM_ putStrLn . reverse . tail . lines =<< getContents 

