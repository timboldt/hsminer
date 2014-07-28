--file: Main.hs
module Main where

import System.Console.ANSI

import Model

main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Miner"
  putStrLn "hello world"


