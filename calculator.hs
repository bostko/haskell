import Data.Char

parseDigits :: [Char] -> Int -> ([Char], Int)
parseDigits "" res = ("", res)
parseDigits (x:xs) res
  |isNumber x = parseDigits xs ((res * 10) + ord x - ord '0')
  |otherwise  = ((x:xs), res)
  
isOperator x = elem x "*+-/"

priority c
  | c == '/' || c == '*' = 3
  | c == '+' || c == '-' = 2

simpleCalc op a b
--  |op == '/' = a / b
  |op == '*' = a * b
  |op == '-' = a - b
  |op == '+' = a + b


-- parseOperatorsAndOperands
--   :: (Eq a, Fractional a1, Fractional a) =>
--       [Char] -> [Char] -> [a] -> a1

parseOperatorsAndOperands
  :: [Char] -> [Char] -> [Int] -> Int

-- parseOperatorsAndOperands [] [] [0.0] = 10.0
parseOperatorsAndOperands [] [] [result] = result
parseOperatorsAndOperands (x:xs) operators operands
  | isOperator x && priority (operators !! 0) >= priority x = parseOperatorsAndOperands xs operators ((simpleCalc (operators !! 0) (operands !! 0) (operands !! 1)):operands)
  | isOperator x && otherwise                 = parseOperatorsAndOperands xs (x:operators) operands
  | isNumber x = parseOperatorsAndOperands (fst (parseDigits (x:xs) 0)) operators ((snd (parseDigits (x:xs) 0)):operands) -- put dollar insteadof brackets, use let parseDigits (x:xs)


calculate :: [Char] -> Int 
calculate str = parseOperatorsAndOperands str [] [] 


