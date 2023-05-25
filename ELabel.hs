-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module ELabel (ELabel (..)) where

import qualified Data.Set   as S
import qualified Data.Maybe as M
import TypesRL

class (Ord a) => ELabel a where
  label2RegEx        :: a -> RegEx
  -- returns a regex, that describes exactly the provided ELabel
  containedPart      :: a -> WordRL -> Maybe (WordRL,WordRL)
  -- Splits a word into the part at the beginning which is contained in a label
  -- and the rest.
  -- Returns Nothing if the label does not accept the word.
  -- Attention:
  -- Always returns Nothing when the word or the label is empty.
  leftAfterTraversal :: a -> WordRL -> Maybe (S.Set WordRL)
  -- Returns Nothing if edge can not be legally traversed with this
  -- word otherwise returns Just the set of all possible remaining
  -- words after traversal.
  -- Except in the case of a RegEx ELabel, this will always be
  -- a singleton set.
  isSingletonLabel   :: a -> Bool
  -- True if the label only contains a single symbol
  isEmptyLabel       :: a -> Bool
  -- True if the label contains the empty word
  singletonToSymbol  :: a -> Maybe SymbolRL
  -- if is no singleton: Nothing
  getAlphabet        :: a -> AlphabetRL
  -- Gets the alphabet used by a label
  word2NFA           :: (Integral b) => WordRL -> NFA b a
  -- makes and NFA that accepts exactly the word provided
  insertEmptyEdge    :: (Ord b) => NFA b a -> b -> b -> NFA b a
  -- Inserts and empty Edge into the nfa pointing from the first provided state
  -- to the second one.
  -- Creates the states if they are not yet in the nfa.
  -- This is trivial for WordRL and RegEx but complicated for SymbolRL
  replaceWordEdge    :: (Integral b) => NFA b a -> TransitionRule b a -> NFA b a
  -- If the second argument is in the delta of the first argument
  -- and if it has not a singleton-label or an empty-label, it gets removed,
  -- and new states and singleton-label-transition-rules get inserted
  -- into the nfa, to create an equivalent nfa.
