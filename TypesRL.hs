-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module TypesRL (makeNFA,SymbolRL,WordRL,emptyWord,AlphabetRL,LanguageRL,TransitionRule,Delta,States,NFA (..),RegEx (..),Grammar (..),getFrom,getTo,getLabel) where

import qualified Data.Set       as S
import qualified Data.Bifunctor as BF (first,second)

type SymbolRL           = Char
type WordRL             = [SymbolRL]
emptyWord               = [] :: WordRL
type AlphabetRL         = S.Set SymbolRL
type LanguageRL         = S.Set WordRL
type TransitionRule a b = ((a,b),S.Set a)
type Delta a b          = S.Set (TransitionRule a b)
type States a           = S.Set a

getFrom :: TransitionRule a b -> a
getFrom ((q,_),_) = q

getTo :: TransitionRule a b -> S.Set a
getTo (_,q) = q

getLabel :: TransitionRule a b -> b
getLabel ((_,a),_) = a

-- NFAs
-- (nondeterministic finite automatons)
--
-- a is the type that is used to label the states of the NFA
-- Usually we want (Ord a).
-- b is the type that is used to label edges of the NFA.
-- Usually we want (Ord b, ELabel b).
data NFA a b = NFA {states :: States a,
                    sigma  :: AlphabetRL,
                    delta  :: Delta a b,
                    start  :: States a,
                    finish :: States a}
                    deriving (Show, Eq)
--
-- Convenient way to create an NFA as it takes lists as arguments instead of sets.
--
-- In the type signature I write [SymbolRL] instead of WordRL, because the argument is
-- not to be understood as a word, but as a list of symbols, that will be translated into a
-- an alphabet i.e. a set of symbols.
makeNFA :: (Ord a, Ord b) => [a] -> [SymbolRL] -> [((a,b),[a])] -> [a] -> [a] -> NFA a b
makeNFA states sigma delta start finish = NFA newStates newSigma newDelta newStart newFinish
  where
    newStates = S.fromList states
    newSigma  = S.fromList sigma
    newDelta  = S.fromList $ map (BF.second S.fromList) delta
    newStart  = S.fromList start
    newFinish = S.fromList finish

-- regular expressions
data RegEx = RE SymbolRL
           | EmptySet
           | EmptyWord
           | Kleene RegEx
           | Concat RegEx RegEx
           | Union RegEx RegEx
           deriving Show

instance Eq RegEx where
  r == r' = show r == show r'
instance Ord RegEx where
  r <= r' = show r <= show r'

-- grammars
-- besides the dataconstructor nothing implemented yet
data Grammar = G {variables   :: AlphabetRL,
                  terminals   :: AlphabetRL,
                  rules       :: S.Set (WordRL,WordRL),
                  startG      :: SymbolRL}
                  deriving (Show, Eq)
