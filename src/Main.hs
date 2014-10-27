import Painting (convertTree2Diagram)
import Parsing (parseTree)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Data.Monoid (mempty)

main = mainWith $ \file -> do
            f <- readFile file
            case parseTree f of
                Left s -> do 
                    putStrLn $ show s
                    return mempty
                Right tree -> return (convertTree2Diagram tree)

            
