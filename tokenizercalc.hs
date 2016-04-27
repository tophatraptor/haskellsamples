--Lab 3
--Jay Dhanoa
module Lab3 (
ArithExpr(Number, Plus, Mult, Div),
leftParen,
rightParen,
isNumber,
eval,
buildExpr,
tokenizer,
parse,
combine
) where

import System.IO
import Data.Char hiding (isNumber)
import Data.List

main :: IO ()
calc=show.eval.buildExpr
main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    putStrLn (calc line)
    main

data ArithExpr = Number Int
  | Plus ArithExpr ArithExpr
  | Mult ArithExpr ArithExpr
  | Div ArithExpr ArithExpr
  | Mod ArithExpr ArithExpr
  --We're not going to actually implement abstract, but we do have negative numbers
  --We also have the luxury of assuming well-formed input, so we don't need to develop cases for throwing errors when it comes to poorly formed input
  deriving (Show, Eq)

isNumber::String->Bool --Extended the functionality of the original isNumber function to an entire String
isNumber []= True
isNumber (x:xs)
  | x=='-' || isDigit(x) = isNumber(xs)
  | otherwise = False


leftParen = '('
rightParen = ')'
--the parentheses matching function of atom mistakes parentheses inside apostrophe/quotation marks for actual code parentheses
eval::ArithExpr->Int
eval (Number n) = n
eval (Plus x y) = eval x + eval y
eval (Mult x y) = eval x * eval y
eval (Div x y) = div (eval x) (eval y)
eval (Mod x y) = mod (eval x) (eval y)

buildExpr::String->ArithExpr
buildExpr = parse . tokenizer

tokenizer::String->[String]
tokenizer [] = []
tokenizer (x:xs)
  | isSpace(x) = tokenizer(xs)
  | isDigit(x) = (takeWhile (isDigit) (x:xs)):tokenizer(dropWhile(isDigit) (xs))
  | x=='-' = ('-':takeWhile (isDigit) xs):tokenizer(dropWhile (isDigit) xs)
  | x==leftParen = takeWhile(/=rightParen) xs:tokenizer(tail(dropWhile (/=rightParen) xs))
  | otherwise =  [x]:tokenizer(xs)


parse::[String]->ArithExpr
parse [] = Number 0
parse (x:xs)
  | elem "+" (x:xs) = Plus (parse(takeWhile (/= "+") (x:xs))) (parse $ tail (dropWhile (/= "+") (x:xs)))
  | elem "*" (x:xs) = Mult (parse(takeWhile (/= "*") (x:xs))) (parse $ tail (dropWhile (/= "*") (x:xs)))
  | elem "/" (x:xs) = Div (parse(takeWhile (/= "/") (x:xs))) (parse $ tail (dropWhile (/= "/") (x:xs)))
  | elem "%" (x:xs) = Mod (parse(takeWhile (/= "%") (x:xs))) (parse $ tail (dropWhile (/= "%") (x:xs)))
  | isNumber(combine (x:xs)) = Number (read(combine (x:xs)))
  | otherwise = parse$ tokenizer$ combine(x:xs)

combine::[String]->String
combine [] = ""
combine (x:xs) = intercalate "" (x:xs)
