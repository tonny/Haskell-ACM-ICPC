{-
   Author : to_On1
   Email  : antonio.mq@gmail.com
   Problem : Is This Integration?
   http://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&category=41&page=show_problem&problem=1150

   Compile and execute with next command
   > ghc --make IsThisIntegration.hs
   > ./IsThisIntegration < isThisIntegration.in

-}
main :: IO()
main = mapM_ putStrLn . integration . lines =<< getContents

integration :: [String] -> [String]
integration [] = []
integration (x:xs) = let arcsen = asin(0.5)
                         a = 1 - ((cos(arcsen) * sin(arcsen))+arcsen)
                         cuad = ((read x)::Float) * ((read x)::Float) 
                         b = 0.5 * cuad * a
                         c = (cuad * ( 1 - pi / 4.0 )) - ( 4.0 * b)
                         re = cuad - (b * 8 + c * 4)
                         ar = c * 4
                         to = b * 8
                         in ((show (precicion re))++" "++(show (precicion ar))++" "++
                            (show (precicion to))) : integration xs

precicion :: Float -> Float
precicion x = fromIntegral (round (x * 1000.0)) / 1000.0
