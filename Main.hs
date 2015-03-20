module Main where

import           Anagrams
import qualified Data.MultiSet      as MS
import qualified Data.Text          as T
import           System.Environment (getArgs)

main :: IO ()
main = do
  dict <- readDict
  [input] <- getArgs
  mapM_ (putStrLn . T.unpack . T.unwords . MS.toList) $ anagrams dict (T.pack input)
