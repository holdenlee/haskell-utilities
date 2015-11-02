module Search where
import System.Environment
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import Data.Array
import Data.Tuple
import Data.Graph.Inductive as G

import Utilities
import ParseUtilities

--warning: does not check if the starting a is the destination
bfs' :: (Ord b) => (a -> [a]) -> (a -> Bool) -> (a -> b) -> [a] -> S.Set b -> Maybe a
bfs' children target summary li visiteds =
    case li of
      [] -> Nothing
      x:rest ->
          let
              cs = children x
              nonrepeats = filter (\x -> not $ S.member (summary x) visiteds) cs
              hits = L.filter target nonrepeats
              newVisiteds = visiteds `S.union` (S.fromList $ map summary nonrepeats)
          in
          --check if we hit the target
            case hits of
              [] ->
                  bfs' children target summary (rest ++ nonrepeats) newVisiteds
              h:rest -> Just h

bfs :: (Ord b) => (a -> [a]) -> (a -> Bool) -> (a -> b) -> a -> Maybe a
bfs children target summary start = bfs' children target summary [start] S.empty

path :: G.Gr a b -> Node -> Node -> (b-> Bool) -> Maybe (Node, [LEdge b]) --([Node], [b])
path g p q test =
    let
        children (v, es) = 
            --lsuc g x gives [(Node,b)]
            --note that order is reversed
            map (\(nd, val) -> (nd, (v,nd,val):es)) $ filter (\(nd,val) -> test val) (lsuc g v)
        target (nd, _) = (nd == q)
        summary = fst
    in
      fmap (mapSnd reverse) (Search.bfs children target summary (p,[]))

--bestfs :: (Eq b, Ord c) => (a -> [a]) -> (a -> Bool) -> (a -> b) -> (a -> c) -> a -> Maybe a
--bestfs children target summary li visiteds heuristic = 
    
