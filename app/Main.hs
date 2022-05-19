module Main where

import Evaluation
import Parsing
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
