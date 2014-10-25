module Tree where

-- data to represent tree with coordinate
data Tree a = Branch { getValue :: a
                      ,getCoord :: (Double, Double)
                      ,getLeaves :: [Tree a]
                     }
    deriving (Show, Eq)
