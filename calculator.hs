import Debug.Trace

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

parseOperatorsAndOperands [] _ [result] = result
parseOperatorsAndOperands [_] _ [result] = result
parseOperatorsAndOperands (x:xs) operators operands
  | isOperator x && (null operators || priority (operators !! 0) >= priority x) && (length operands) > 1 = parseOperatorsAndOperands xs (tail operators) ((simpleCalc (operators !! 0) (operands !! 0) (operands !! 1)):operands)
  | isOperator x && not ((null operators || priority (operators !! 0) >= priority x) && (length operands) > 1) = parseOperatorsAndOperands xs (x:operators) operands
  | isNumber x = parseOperatorsAndOperands (fst (parseDigits (x:xs) 0)) operators ((snd (parseDigits (x:xs) 0)):operands) -- put dollar insteadof brackets, use let parseDigits (x:xs)
  | otherwise = operands !! 0 

calculate :: [Char] -> Int 
calculate str = parseOperatorsAndOperands str "" [] 


