module Main where

import           Anagrams
import qualified Data.Text          as T
import           System.Environment (getArgs)

main :: IO ()
main = do
  dict <- readDict
  [input] <- getArgs
  mapM_ (putStrLn . T.unpack) $ anagrams dict (T.pack input)
