module Main where

import Lib

main :: IO ()
main = interact $ unlines . map jsonToWExprJson . lines
