module Main where

import System.Environment (getArgs, getProgName)
import qualified HR.SavePrincess
import qualified HR.SavePrincess2


challenges =
  [ ("saveprincess"  , HR.SavePrincess.main)
  , ("saveprincess2" , HR.SavePrincess2.main)
  ]

usageMsg :: IO String
usageMsg = do
  let cMsg = unlines $ fmap ((" - " ++) . fst) challenges
  progName <- getProgName
  return ("Usage: " ++ progName ++ " CHALLENGE_NAME\n\
          \where CHALLENGE_NAME is one of:\n" ++ cMsg)

main = do
    args <- getArgs
    case args of
      [x] -> maybe usage id $ lookup x challenges
      _   -> usage
  where
    usage = putStr =<< usageMsg
