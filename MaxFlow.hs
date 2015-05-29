module MaxFlow where
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
import Search
import GraphUtils

import Utilities
import ParseUtilities

--returns both the flow graph and the max flow.
--NOTE: g should have been initiated in both ways
fordFulkerson :: (Num b, Ord b) => G.Gr a b -> Node -> Node -> (G.Gr a b, b)
fordFulkerson g s t = 
    let
        --zero out edges in flow graph
        startf = G.emap (const 0) g
        {-startf = gmap (\(adj1, v, adj2) -> 
                           let 
                               adjs = mapSnd (const 0) $ union adj1 adj2
                           in
                             (adjs, v, adjs))-}
        (_,f',flow',_) = while (\(gf,f,flow,b) -> b) 
                            (\(gf,f,flow,b) ->
                                 let
                                     pth = path gf s t (>0) --`debug` (show gf ++ show (path gf s t (>0)))
                                 in 
                                   case pth of
                                     Nothing -> (gf, f, flow, False) --no path exists, terminate
                                     Just (_,es) -> 
                                         let 
                                             cf = minimum $ map third es
                                            --edges with vertices only (no labels)
                                             esV = map (\(x,y,z) -> (x,y)) es
                                             revEs = map swap esV
                                             fnew = f |> modEdges esV (+cf) |> modEdges revEs (\x -> x-cf)
                                             gfnew = gf |> modEdges esV (\x -> x-cf) |> modEdges revEs (+cf)
                                          in
                                            (gfnew, fnew, flow + cf, True))
                            (g, startf, 0, True)
    in
      (f',flow')
