-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module SymbolRL () where

import ELabel
import TypesRL
import ShowRL
import WordRL
import qualified Data.Set as S

instance ShowRL SymbolRL where
  showRL = show

instance ELabel SymbolRL where
  label2RegEx :: SymbolRL -> RegEx
  label2RegEx = RE

  containedPart :: SymbolRL -> WordRL -> Maybe (WordRL,WordRL)
  containedPart _ [] = Nothing
  containedPart label (x:xs)
    | label == x = Just ([x],xs)
    | otherwise  = Nothing

  leftAfterTraversal :: SymbolRL -> WordRL -> Maybe (S.Set WordRL)
  leftAfterTraversal _ [] = Nothing
  leftAfterTraversal label (x:xs)
    | label == x = Just $ S.singleton xs
    | otherwise  = Nothing

  isSingletonLabel :: SymbolRL -> Bool
  isSingletonLabel _ = True

  singletonToSymbol :: SymbolRL -> Maybe SymbolRL
  singletonToSymbol = Just

  isEmptyLabel :: SymbolRL -> Bool
  isEmptyLabel _ = False

  getAlphabet :: SymbolRL -> AlphabetRL
  getAlphabet    = S.singleton

  word2NFA :: (Integral b) => WordRL -> NFA b SymbolRL
  word2NFA word = makeNFA [0..lengthIntegral] (S.toList $ getAlphabet word) delta [0] [lengthIntegral]
    where
      lengthIntegral = fromInteger $ toInteger $ length word
      delta = zip (zip [0..lengthIntegral] word) (map (:[]) [1..lengthIntegral])

  insertEmptyEdge :: (Ord b) => NFA b SymbolRL -> b -> b -> NFA b SymbolRL
  insertEmptyEdge nfa q q' = nfa {delta = newDelta}
    where
      newDelta = S.union (delta nfa)
               $ S.map (\((_,s),rs) -> ((q,s),rs))
               $ S.filter ((==q') . getFrom)
               $ delta nfa

  replaceWordEdge :: (Integral a) => NFA a SymbolRL -> TransitionRule a SymbolRL -> NFA a SymbolRL
  replaceWordEdge nfa _ = nfa
