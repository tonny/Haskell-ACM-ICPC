{--
 - Author : Antonio Mamani
 - Description : Convert integer to roman numerical
 - version : 26022010
--}

entero = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
romano = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

convert :: Int -> String 
convert  num | num > 0 && num < 4000 = roman 0 num
             | otherwise = "Numero fuera de rango" 
	    where 
	    roman _ 0 = ""
	    roman p n | n >= entero !! p = romano !! p ++ roman p (n-entero!!p)
	              | otherwise = roman (p+1) n
