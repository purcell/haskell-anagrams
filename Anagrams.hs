{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.Char           (isSpace)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Tree           as Tr

type Word = Text
type Dictionary = Set Word

readDict :: IO Dictionary
readDict = (S.filter goodWord . S.fromList . T.lines) <$> TIO.readFile "/usr/share/dict/words"

goodWord :: Word -> Bool
goodWord "A" = True
goodWord "I" = True
goodWord "O" = True
goodWord w   = T.length w > 1

type Anagram = MultiSet Word
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- The anagrams with the fewest words are returned first, which can lead to
-- high memory usage.
anagrams :: Dictionary -> Word -> [Text]
anagrams dict source = extractAnagrams $ breadthFirstNodes $ Tr.unfoldTree expand initialState
  where breadthFirstNodes = concat . Tr.levels
        initialState = (MS.empty, wordLetters source, dict)

extractAnagrams :: [SearchState] -> [Text]
extractAnagrams = map (\(ana, _, _) -> T.unwords $ MS.toList ana) . filter noLettersRemaining
  where noLettersRemaining (_, remaining, _) = MS.null remaining

expand :: SearchState -> (SearchState, [SearchState])
expand anagram@(wordsSoFar, remaining, dict) = (anagram, nextStates)
  where
    possibleWords = S.filter (remaining `canSpell`) dict
    nextStates = fst $ foldl go ([], possibleWords) $ S.toList possibleWords
    go (states, d) word =
      ((MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d):states,
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters

wordLetters :: Word -> Letters
wordLetters = MS.fromList . filter (not . isSpace) . T.unpack

