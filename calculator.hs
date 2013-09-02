import Data.Char
calculate :: [Char] -> Fractional a => a 
calculate "" = 0.0

-- parseNext :: [Char] -> (Fractional a => a)

parseDigits "" res = res
parseDigits (x:xs) res = parseDigits xs ((res * 10) + ord x - ord '0')  
