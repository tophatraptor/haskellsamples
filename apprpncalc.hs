module Lab5 where

import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser result = Parser { runParser :: String -> [(String, result)] }

failure :: Parser r
failure = Parser $ const []

succeed :: r -> Parser r
succeed v = Parser $ \stream -> [(stream, v)]

instance Functor Parser where
  fmap f (Parser pattern) = Parser $ (fmap . fmap . fmap) f pattern


instance Applicative Parser where
  pure result = succeed result
  Parser pattern_map <*> Parser pattern
    = Parser $ \s -> [(u, f a) | (t, f) <- pattern_map s, (u, a) <- pattern t]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  []   -> []
  a:as
    | p a -> [(as, a)]
    | otherwise -> []

char :: Char -> Parser Char
char = satisfy . (==)

isOp :: Char -> Bool
isOp x
  | x=='+' || x=='*' || x=='%' || x=='/' || x=='-' || x=='^' || x=='|'= True
  | otherwise = False

leftParen ='('
rightParen = ')'

isParen :: Char -> Bool
isParen x
  | x==leftParen || x==rightParen = True
  | otherwise = False

isANumber x
  | isDigit x || x=='-' = True
  | otherwise = False

isASpace x
  | isSpace x || x==rightParen = True
  | otherwise = False

alpha = satisfy isAlpha
digit = satisfy isANumber
space = satisfy isASpace
op = satisfy isOp
paren = satisfy isParen

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

number :: Parser [Char]
number = (fmap (:) digit) <*> (number <|> succeed [])

ops :: Parser [Char]
ops = (fmap (:) op) <*> (succeed [])

input :: Parser [String]
input = undefined
factorial :: Integer -> Integer
factorial x = product [1..x]

instance Alternative Parser where
  empty = Parser $ const []
  Parser pattern1 <|> Parser pattern2 = Parser $ liftA2 (++) pattern1 pattern2

end :: Parser ()
end = Parser $ \stream -> [(stream, ()) | null stream]

just :: Parser r -> Parser r
just pattern = const <$> pattern <*> end

(.>) :: Parser r1 -> Parser r2 -> Parser r2
parser1 .> parser2 = fmap (flip const) parser1 <*> parser2

(<.) :: Parser r1 -> Parser r2 -> Parser r1
parser1 <. parser2 = fmap const parser1 <*> parser2

(<?>) :: (r -> Bool) -> Parser r -> Parser r
predicate <?> (Parser parser)
  = Parser $ \s -> [(t, r) | (t, r) <- parser s, predicate r]

someSpaces = (fmap (flip const) space) <*> (someSpaces <|> succeed "")
someParens = (fmap (flip const) paren) <*> (someParens <|> succeed "")
someOps = (fmap (flip const) op) <*> (someOps <|> succeed "")

spaces = succeed "" <|> someSpaces
parens = succeed "" <|> someParens

numberWithSpaces = spaces .> number <. spaces
numberWithParens = spaces .> parens .> spaces .> number <. spaces <. parens <. spaces
opsWithSpaces = spaces .> ops <. spaces

numbers = (fmap (:) numberWithSpaces) <*> (numbers <|> succeed [])
multops = (fmap (:) opsWithSpaces) <*> (multops <|> succeed [])
negnumbers = (fmap (:) numberWithParens) <*> (negnumbers <|> succeed [])

parseRPN' = ((fmap (++) numbers) <|> (fmap (++) multops) <|> (fmap (++) negnumbers)) <*> (parseRPN' <|> succeed [])

parseRPN = just parseRPN'

solveRPN :: [String] -> Integer
solveRPN  = head . foldl foldingFunction []
  where
    foldingFunction (x:y:ys) "*" = ((*) x y):ys
    foldingFunction (x:y:ys) "+" = ((+) x y):ys
    foldingFunction (x:xs) "|" = (abs x):xs
    foldingFunction (x:y:ys) "-" = ((-) y x):ys
    foldingFunction (x:y:ys) "%" = (mod y x):ys
    foldingFunction (x:y:ys) "/" = (div y x):ys
    foldingFunction (x:ys) "!" = (factorial x):ys
    foldingFunction (x:y:ys) "^" = ((^) y x):ys
    foldingFunction xs numberString = (read (numberString) :: Integer):xs
