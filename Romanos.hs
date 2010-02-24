module Romano where
{-
 - Author: Antonio Mamani
 - Problem : Convert an Integer to a roman numerical
 - version : 24022010
 - -}
convertir :: Int -> String
convertir num | num > 0 && num < 10 = numeros !! num  
              | num-10 < 30      = diez num
	      | num-50 < 40      = if num-50 < 0 then "XL"++ numeros!!(num `mod` 10) else cincuenta num
              | num-100 < 300    = if num-100 < 0 then "XC"++ numeros!!(num `mod` 10) else cien num
	      | num-500 < 400    = if num-500 < 0 then "CD"++ convertir (num `mod` 100) else quinientos num  
	      | num-1000 < 99899 = if num-1000 < 0 then "CM"++convertir (num `mod` 100) else mil num  
	      | otherwise = "numero fuera de rango"
	      where
              diez n = concat (map (\x->"X") [1..(n `div` 10)]) ++ numeros!!(n `mod` 10) 

              cincuenta n = "L" ++ diez(n-50)

              cien n = concat (map (\x->"C") [1..(n `div` 100)]) ++ convertir (n `mod` 100) 

              quinientos n = "D" ++ convertir (n-500)

              mil n = concat(map (\x->"M") [1..(n `div` 1000)]) ++ convertir (n `mod` 1000) 

numeros = ["","I","II","III","IV","V","VI","VII","VIII","IX"]

