-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module WordRL () where

import qualified Data.Set   as S
import qualified Data.Maybe as M
import TypesRL
import ELabel
import ShowRL

instance ShowRL WordRL where
  showRL = show

instance ELabel WordRL where
  label2RegEx :: WordRL -> RegEx
  label2RegEx []   = EmptyWord
  label2RegEx word = foldr1 Concat $ map RE word

  containedPart :: WordRL -> WordRL -> Maybe (WordRL,WordRL)
  containedPart _     emptyWord = Nothing
  containedPart label word
    | isEmptyLabel label            = Nothing
    | lengthMatching == lengthLabel = Just (label,drop lengthLabel word)
    | otherwise                     = Nothing
    where
      lengthLabel    = length label
      lengthMatching = length
                     $ takeWhile id
                     $ zipWith (==) label word

  leftAfterTraversal :: WordRL -> WordRL -> Maybe (S.Set WordRL)
  leftAfterTraversal label word
    | M.isNothing parts = Nothing
    | otherwise         = Just $ S.singleton $ snd $ M.fromJust parts
    where
      parts = containedPart label word

  isSingletonLabel :: WordRL -> Bool
  isSingletonLabel word = length word == 1

  singletonToSymbol :: WordRL -> Maybe SymbolRL
  singletonToSymbol [w] = Just w
  singletonToSymbol _   = Nothing

  isEmptyLabel :: WordRL -> Bool
  isEmptyLabel = (==) emptyWord

  getAlphabet :: WordRL -> AlphabetRL
  getAlphabet = S.fromList

  word2NFA :: (Integral b) => WordRL -> NFA b WordRL
  word2NFA word = makeNFA [0,1] (S.toList $ getAlphabet word) [((0,word),[1])] [0] [1]

  insertEmptyEdge :: (Ord b) => NFA b WordRL -> b -> b -> NFA b WordRL
  insertEmptyEdge nfa q q' = nfa {states = newStates,
                                  delta  = newDelta}
    where
      newStates = S.insert q $ S.insert q' $ states nfa
      newDelta  = S.insert ((q,""),S.singleton q') $ delta nfa

  replaceWordEdge :: (Integral a) => NFA a WordRL -> TransitionRule a WordRL -> NFA a WordRL
  replaceWordEdge nfa edge
    | lengthEdgeLabel <= 1            = nfa
    | not $ S.member edge (delta nfa) = nfa
    | otherwise                       = nfa {states = newStates,
                                             delta  = newDelta}
      where
        lengthEdgeLabel = fromInteger $ toInteger $ length $ getLabel edge
        newStatesStart  = 1 + S.findMax (states nfa)
        newStates  = S.fromList [newStatesStart..newStatesStart + lengthEdgeLabel]
        newDelta   = S.union addedDelta
                   $ S.delete edge
                   $ delta nfa
        addedDelta = S.fromList
                   $ makeNewEdges [((getFrom edge, [head $ getLabel edge]),S.singleton newStatesStart)]
                                  (newStatesStart + 1)
                                  (tail $ getLabel edge)
        makeNewEdges accEdges n [w]    = ((n-1,[w]),getTo edge) : accEdges
        makeNewEdges accEdges n (w:ws) = makeNewEdges (((n-1,[w]),S.singleton n) : accEdges) (n+1) ws
