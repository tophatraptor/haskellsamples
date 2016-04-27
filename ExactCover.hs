module Main where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.List (intersect, nub, foldl, union, intercalate, sort, group)
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Maybe

import NState

type Element = String
type Set = [Element]
type Collection = [Set]
type Cover = Map Element Set
type Solver = Cover -> [Cover]

extend :: Collection -> Element -> NState Cover ()
extend col e = do
  cover <- get
  val <- branch $ viable col e cover
  put $ allInserts val val cover

allInserts :: Set -> Set -> Cover -> Cover
allInserts [] x c1 = c1
allInserts (x:xs) s2 c1 = allInserts xs s2 (Map.insert x s2 c1)

viable :: Collection -> Element -> Cover -> Collection
viable [] a cov = []
viable (x:xs) a cov = nub result where
  result = if (Map.lookup a cov == Nothing) then
    if (or(fmap (flip elem (Map.keys cov)) x))
      then viable xs a cov
      else x:(viable xs a cov)
  else [[]]

backtrack :: Collection -> Solver
backtrack col = execNState (mapM_ (extend col) (universe col))

universe :: Collection -> [Element]
universe (x:xs) = nub $ concat (x:xs)

makeCollection :: String -> Collection
makeCollection text = fmap words $ lines text

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

--altered runSolver by adding nub to the case statement
runSolver :: Collection -> String
runSolver collection = case nub $ backtrack collection Map.empty of
    [x] -> (++) "1 solution\n" $ show x
    []  -> "0 solutions"
    xs  -> (((++) $ show $ length xs) " solutions\n") ++ (intercalate "\n" . map show $ xs)


main :: IO ()
main = do
    collection <- fmap makeCollection getContents
    putStr . unlines $
        [ "problem:"
        , ""
        , indent 3 $ (++) "Elements: " $ show $ universe collection
        , indent 3 $ (++) "Sets: " $ show collection
        , ""
        , "solutions:"
        , ""
        , indent 3 $ runSolver collection
        ]
