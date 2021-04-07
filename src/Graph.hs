module Graph
( Edge
, Graph
, mkGraph
, insertNode
, insertVertice
) where

import Data.Map(Map)
import Data.Map as Map
import Data.List as List

-- Types
data Edge id = Lnk id id
data Graph id a = G (Map id a) [Edge id]


-- Creation
mkGraph :: (Ord id) => [(id, a)] -> [(id, id)] -> Graph id a
mkGraph nodes vertices =
  let edges = List.map (\(x, y) -> Lnk x y) vertices in
  G (Map.fromList nodes) edges


-- Display
instance (Show id) => Show (Edge id) where
  show (Lnk a b) = (show a) ++ " -> " ++ (show b)

instance (Eq id, Show id, Show a) => Show (Graph id a) where
  show (G nodes edges) =
    let foldFun acc x = acc ++ ('\t' : (show x)) ++ "\n"
        show_edges = List.foldl' foldFun "Edges:\n" edges in
    let foldFun acc k item = acc ++ ('\t' : (show item)) ++ " (" ++ (show k) ++ ")\n"
        show_nodes = Map.foldlWithKey foldFun "Nodes:\n" nodes in
    show_nodes ++ show_edges


-- Manipulation
insertNode :: (Ord id) => id -> a -> Graph id a -> Graph id a
insertNode id item (G nodes edges) = G (Map.insert id item nodes) edges

insertVertice :: (Ord id) => id -> id -> Graph id a -> Graph id a
insertVertice id id' (G nodes edges) =
  let vertice = Lnk id id' in
  G nodes (vertice : edges)
