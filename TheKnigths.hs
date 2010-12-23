{-
  Author  : to_On1
  Email   : antonio.mq@gmail.com
  Problem : The Knights of the round table 
  http://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&category=41&page=show_problem&problem=1136

  Instruction :
  Compile and execute with next command
  > ghc --make TheKnights.hs
  > ./TheKnights < theKnights.in
-}
main :: IO()
main = mapM_ putStrLn . knight . lines =<< getContents

knight :: [String] -> [String]
knight [] = []
knight (p:xs) = let region = map (\j -> (read j)::Float) (words p)
                    x = region !! 0 
                    y = region !! 1
                    z = region !! 2
                    a = acos((( y^2 +  z^2) -  x^2) / (2 * y * z))
                    b = asin( y * (sin a / x ))
                    c = asin( z * (sin a / x ))
                in ( "The radius of the round table is: " ++ 
                   show (( (x + y + z) / ((2 / tan(a / 2)) + (2 / tan(b / 2)) + (2 / tan(c / 2))))) ) : knight xs
