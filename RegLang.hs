-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module RegLang (
                RegLang,
                accepts,
                toList,
                fromList,
                toDFA,
                toWordNFA,
                fromNFA,
                addWord,
                union,
                concatenate,
                kleene,
                intersection,
                difference,
                simplify
               ) where

import TypesRL
import ELabel

class RegLang a where
  accepts      :: a -> WordRL -> Maybe Bool
  -- Checks if a word is inside of the language represented
  -- by the argument.
  -- Returns Nothing if the word contains symbols which
  -- are not part of the language's alphabet.
  toList       :: a -> [WordRL]
  -- Returns a (possibly infinite) list of the words
  -- of the represented language.
  fromList     :: [WordRL] -> a
  -- Takes a (finite) list of words and creates an
  -- according language representation.
  toDFA        :: (Integral b) => a -> NFA b SymbolRL
  -- Returns a DFA (NFA with special properties) that
  -- represents the same language as the input.
  toWordNFA    :: (Integral b) => a -> NFA b WordRL
  -- Returns an NFA that allows words as labels that
  -- represents the same language as the input.
  fromNFA      :: (Integral b, ELabel c) => NFA b c -> a
  -- Takes an NFA and returns an equivalent language representation
  -- of choice.
  addWord      :: a -> WordRL -> a
  -- Extends a language representation in the way, that
  -- the represented language also contains the provided word.
  union        :: a -> a -> a
  -- Returns the representation of a language, that is the union
  -- of the languages represented by the input.
  concatenate  :: a -> a -> a
  -- Returns the representation of a language, that is the concatenation
  -- of the languages represented by the input.
  kleene       :: a -> a
  -- Returns the representation of a language, that one gets,
  -- when applying the kleene-star to the language represented
  -- by the input
  intersection :: a -> a -> a
  -- Returns the representation of a language, that is the intersection
  -- of the languages represented by the input.
  difference   :: a -> a -> a
  -- Returns the representation of a language, that is the set-difference
  -- of the languages represented by the input.
  simplify     :: a -> a
  -- tries to change the language's representation into an
  -- equivalent but simpler one.
  -- What exactly is done depends on the type of representation.
