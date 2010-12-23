{-
  Author  : to_On1
  Email   : antonio.mq@gmail.com
  Problem : The Largest/Smallest Box
  http://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&category=41&page=show_problem&problem=1156

  Instruction :
  Compile and execute with next command
  > ghc --make LargestSmallest.hs
  > ./LargestSmaller < largestSmaller.in
-}
main :: IO()
main = mapM_ putStrLn . box . lines =<< getContents

box::[String]->[String]
box [] = []
box (x:xs) = let punto = map (\j->(read j)::Float) (words x)
                 a = punto !! 0
                 b = punto !! 1
                 p = sqrt ((16.0 * (b + a)^2) - (48.0 * b * a))
             in ( show (((4.0 * (b + a)) - p) / 24.0) ++ " 0.000 "++ show ((min a b) / 2.0)) : box xs
