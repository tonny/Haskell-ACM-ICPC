module Main where
import Char
{--
 - Author      : Antonio Mamani Quispe
 - version     : 332010
 - exercise    : http://www.spoj.pl/problems/ARITH/
 - description : Simple Arithmetics
 - --}

main :: IO()
main = do leer <- getLine
          putStrLn (resolv leer)

resolv [] = []
resolv exp = let (first,resto)   = span isDigit exp     
             in let (sig,second) = (head resto,tail resto)
             in let (num1,num2)  = (((read first)::Integer),((read second)::Integer))
             in sol sig num1 num2 first second
             where 
             sol o n1 n2 f s | o=='+'=" "++f++"\n"++"+"++s++"\n"++map(\x->'-')[0..(length(show(n1+n2)))]++"\n"++" "++show(n1+n2)
                            | o=='-'=" "++f++"\n"++"-"++s++"\n"++map(\x->'-')[0..(length f)]++"\n"++" "++show(n1-n2)
                            | o=='*'= printMul n1 n2 f s (length s) (length (show(n1*n2)))
			    | otherwise = "error"
             
	     printMul :: Integer -> Integer -> String -> String -> Int -> Int -> String
             printMul _ _ _ _ 0 _   = ""
             printMul n1 n2 f s c t = let esp = if t > (length (f++s)) then 1 else 0
	                              in if n1>n2 then space (t-(length f)+esp)++show n1++"\n"++space(t-(length s)-1+esp)++"*"++show n2++"\n"++map(\x->'-')[0..t]++"\n"++show (n1*n2) else ""

             space 0 = ""
             space n = " "++space(n-1)


