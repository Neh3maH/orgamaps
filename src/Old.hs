module Old () where

import Graph
import Data.List as List
import Json

defGraph :: Graph Int String
defGraph =
  -- Ids
  let id0 = 0 in
  let id1 = 1 in
  -- Nodes
  let n0 = (id0, "a") in
  let n1 = (id1, "b") in
  -- Edges
  let e0 = (id0, id1) in
  ---- Graph
  let nodes = [n0, n1] in
  let edges = [e0] in
  mkGraph nodes edges

someFunc' :: IO ()
someFunc' =
  let graph = defGraph in
  let op0 = insertNode 2 "new node overwritten"
      op1 = insertNode 2 "new node"
      op2 = insertVertice 0 2
      opList = [op0, op1, op2] in
  let foldf g op = op g
      graphTransform g = List.foldl' foldf g opList in
  let f = putStr . show . graphTransform in
  f graph
