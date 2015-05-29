{-# OPTIONS
 
 -XMultiParamTypeClasses
 -XFunctionalDependencies
 -XFlexibleInstances
 -XRank2Types
 -XGADTs
 -XPolyKinds
#-}

module Main where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array

import Utilities
import ParseUtilities

dp :: (Ord k) => (k -> [k]) -> (k -> [v] -> v) -> ((M.Map k v, k) -> (M.Map k v, v))
dp genSimpler combine (m,x) = 
--NOTE: does not take advantage of lazy eval
    let 
        dp' = dp genSimpler combine
        (newM, vals) = L.mapAccumL (curry dp') m (genSimpler x)
        ans = combine x vals
    in
      case M.lookup x m of
        Nothing -> (newM |> M.insert x ans, ans)
        Just y -> (m, y)

dpmap :: ((M.Map k v, k) -> (M.Map k v, v)) -> [k] -> [v]
dpmap f ks = snd $ L.mapAccumL (curry f) M.empty ks

dp2 :: (Ord k) => (k -> [k]) -> (k -> [v] -> v) -> k -> v
dp2 genSimpler combine x = snd $ dp genSimpler combine (M.empty,x)

--example
fib :: Int -> Int 
fib = dp2 (\n -> if n<=1 then [] else [n-1,n-2]) (\n vs -> if n==0 then 0 else if n==1 then 1 else vs!!0+vs!!1)

type Input = Int
type Output = Int

input :: String -> [Input]
input s = map read $ keepi (>=2) (lines s)

calc :: Input -> Output
calc = fib

output :: [Output] -> String
output li = 
  unlines (emap (\(x,y) -> "Case #"++(show x)++": "++(show y)) li)

main:: IO ()
main = do
  args <- getArgs
  let inputF = if (length args >= 1) then args !! 0 else "a.in"
  let outputF = if (length args >= 2) then args !! 1 else (take (length inputF - 2) inputF ++ "out")
  ioFile inputF outputF (\s -> output $ map calc (input s))
