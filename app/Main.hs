module Main where

import WSQL

main :: IO ()
main = interact $ unlines . map jsonToWExprJson . lines
