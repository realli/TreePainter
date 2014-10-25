module Parsing (parseTree) where

import Text.ParserCombinators.Parsec
import Tree

-- a tree parser, may be a little tidious
tree :: GenParser Char st (Tree String)
tree = do
    spaces
    value <- many1 (noneOf " \n[]") <?> "node value"
    spaces
    x <- many1 digit <?> "x coord"
    spaces
    y <- many1 digit <?> "y coord"
    spaces
    char '[' <?> "[ -- begin of leaves"
    spaces
    leaves <- sepBy tree (char ',')
    spaces
    char ']' <?> "] -- end of leaves"
    return $ Branch value (read x, read y) leaves

parseTree :: String -> Either ParseError (Tree String)
parseTree input = parse tree "parse error" input
