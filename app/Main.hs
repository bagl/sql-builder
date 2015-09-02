module Main where

import WSQL (jsonToWExprJson)

main :: IO ()
main = interact $ unlines . map jsonToWExprJson . lines
