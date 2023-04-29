-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module FA () where

import qualified Data.Set as S (Set, fromList, isSubsetOf, cartesianProduct, empty, map, filter)
import qualified Data.Maybe as M () 
import qualified Data.Bifunctor as BF (second)

-- TransitionRule uses [Symbol] instead of Symbol to make ε-labeled arrows possible by supplying
-- an empty list.
-- The [Symbol] part of a TransitionRule should always be a singleton list or empty. 
type Symbol           = Char
type Alphabet         = S.Set Symbol
type Language         = S.Set [Symbol]
type TransitionRule a = ((a,[Symbol]),S.Set a)
type Delta a          = S.Set (TransitionRule a)
type States a         = S.Set a

data FA a = FA {states :: States a, 
                sigma :: Alphabet,
                delta :: Delta a,
                start :: States a,
                finish :: States a}
                deriving (Show, Eq)

-- Convinient way to create an FA as it takes lists as arguments instead of sets.
--
makeFA :: (Ord a) => [a] -> [Symbol] -> [((a,[Symbol]),[a])] -> [a] -> [a] -> Maybe (FA a)
makeFA states sigma delta start finish
  | validFA newFA = Just newFA
  | otherwise     = Nothing
  where
    newFA = FA newStates newSigma newDelta newStart newFinish
    newStates = S.fromList states
    newSigma  = S.fromList sigma
    newDelta  = S.fromList $ map (BF.second S.fromList) delta
    newStart  = S.fromList start
    newFinish = S.fromList finish

-- Checks if an FA is defined correctly.
-- Returns False if any of the following holds
-- - start is no subset of states
-- - finish is no subset of states
-- - not all left entries of delta have their left entries in states or their right entry as an empty
--   list or with a single argument, that is in sigma
-- - the set of the right entries of delta is not a subset of states
-- Otherwise returns True.
--
-- Needs general rewrite also to handle ε-lableded arrows
validFA :: (Ord a) => FA a -> Bool
validFA inputFA = S.isSubsetOf (start  inputFA) (states inputFA) &&
                  S.isSubsetOf (finish inputFA) (states inputFA) &&
                  S.isSubsetOf deltaLeft (S.cartesianProduct (states inputFA) (sigma inputFA)) -- &&
                  --S.empty (S.filter (not . (`S.isSubsetOf` (states inputFA))) deltaRight)
  where
    deltaLeft  = S.map ((\(arg,[val]) -> (arg,val)) . fst) (delta inputFA)
    deltaRight = S.map snd (delta inputFA)

acceptsFA :: (Ord a) => FA a -> [Symbol] -> Maybe Bool
acceptsFA = undefined
