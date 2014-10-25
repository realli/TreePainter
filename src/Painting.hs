{-# LANGUAGE NoMonomorphismRestriction #-}
module Painting where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Tree

-- type alias
type Label = Integer
type Graph = Diagram B R2

-- paint one node with name in it
node :: (Show a) => a -> Label -> Graph
node n l = text (show n) # fontSizeN 0.1 # fc red
           <> circle 0.2 # fc green # named l

-- convert Tree to tuple of Graph and Label
-- take an Label as initial, return the next availble Label also
-- Label is used to name graph node, so we can draw line between
-- each tree nodes
paintTree :: (Show a) => Tree a -> Label -> (Graph, Label)
paintTree (Branch value coord leaves) start =
                            foldl go (paintNode, start + 1) leaves
    where paintNode = node value start # moveTo (p2 coord)
          go (ms, init) ele = let elePainting = paintTree ele init
                                  pos = p2 . getCoord $ ele
                              in (connectOutside start init $
                                      fst elePainting <> ms, snd elePainting)

-- convert Tree to Graph
convertTree2Diagram :: (Show a) => Tree a -> Graph
convertTree2Diagram tr = let (g, _) = paintTree tr 1
                         in g

main = mainWith (convertTree2Diagram $ Branch 1 (-2,-2) [(Branch 2 (-3,-3) []), (Branch 3 (-1,-3) [])])
