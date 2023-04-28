-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module DFA (DFA, makeDFA, validDFA, accepts) where

import qualified Data.Set as S
import Data.Maybe (fromJust, isNothing)
    
type Symbol = Char
type WordDFA = [Symbol]
type TransitionRule a = ((a,Symbol),a)
type TransitionFunction a = S.Set (TransitionRule a)

-- represents a DFA
--
-- as all DFA-functions expect the type variable to be of Ord
-- it is reccomended to declare DFAs of type Ord
data DFA a = DFA {states :: S.Set a, 
                  sigma :: S.Set Symbol,
                  delta :: TransitionFunction a,
                  q0 :: a,
                  f :: S.Set a}
                  deriving (Show, Eq)

-- returns always succeeding Maybe value to keep consistency with the other DFA operations
complementDFA :: Ord a => DFA a -> Maybe (DFA a)
complementDFA dfa = Just $ DFA (states dfa) (sigma dfa) (delta dfa) (q0 dfa) (S.difference (states dfa) (f dfa))

-- returns Nothing when first and second arguments have different alphabets i.e. sigma
intersectionDFA :: (Ord a, Ord b) => DFA a -> DFA b -> Maybe (DFA (a,b))
intersectionDFA dfa dfb
  | sigma dfa /= sigma dfb = Nothing
  | otherwise              = Just $ DFA statesNew sigmaNew deltaNew q0New fNew
    where
      statesNew = S.cartesianProduct (states dfa) (states dfb)
      sigmaNew = sigma dfa
      deltaNew = S.map (\(q,s) -> ((q,s),
                                   (fromJust $ applyTransitionFunction (delta dfa) (fst q) s,
                                    fromJust $ applyTransitionFunction (delta dfb) (snd q) s)))
                       (S.cartesianProduct statesNew sigmaNew)
      q0New = (q0 dfa, q0 dfb)
      fNew = S.cartesianProduct (f dfa) (f dfb)

unionDFA :: (Ord a, Ord b) => DFA a -> DFA b -> Maybe (DFA (a,b))
unionDFA dfa dfb = do dfa' <- complementDFA dfa
                      dfb' <- complementDFA dfb
                      dfc  <- intersectionDFA dfa' dfb'
                      complementDFA dfc

-- Although the entries of a DFA are represented as Sets from the module Data.Set
-- this function can be used to simplify the creation of a new DFA by passing lists intead of sets.
-- It returns Nothing if the result is not a valid DFA, which is checked with validDFA.
makeDFA :: Ord a => [a] -> [Symbol] -> [TransitionRule a] -> a -> [a] -> Maybe (DFA a)
makeDFA states sigma delta q0 f
  | validDFA dfa = Just dfa
  | otherwise    = Nothing
    where
      dfa = DFA (S.fromList states) (S.fromList sigma) (S.fromList delta) q0 (S.fromList f)

-- Returns False if the Argument does not represent a valid DFA.
-- This can be for one of the following reasons.
--   states or sigma is empty
--   the domain of delta is not the cartesian product of states and sigma
--   the range of delta is not a subset of sigma
--   delta is not right unique
--   q0 is not in states
--   f is not a subset of states
validDFA :: Ord a => DFA a -> Bool
validDFA dfa = not (S.null $ states dfa) &&
               not (S.null $ sigma dfa) &&
               S.member (q0 dfa) (states dfa) &&
               S.isSubsetOf (f dfa) (states dfa) &&
               S.isSubsetOf domainDelta cartesianQSigma &&
               not (S.isProperSubsetOf domainDelta cartesianQSigma) &&
               S.isSubsetOf deltaRange (states dfa) &&
               S.size (delta dfa) == S.size (states dfa) * S.size (sigma dfa)
  where
    deltaRange = S.map snd (delta dfa)
    domainDelta = S.map fst (delta dfa)
    cartesianQSigma = S.cartesianProduct (states dfa) (sigma dfa)

-- Returns nothing, if the tuple of the second and the third argument ist not
-- in the domain of the first argument.
applyTransitionFunction :: Ord a => TransitionFunction a -> a -> Symbol -> Maybe a
applyTransitionFunction delta q s
  | isNothing i = Nothing
  | otherwise   = Just $ snd $ S.elemAt (fromJust i) delta
    where i = S.lookupIndex (q,s) (S.map fst delta)

-- Returns Nothing if the second Argument is not composed of the first argument's alphabet.
-- Just True if first argument accepts second argument
-- Just False otherwise
accepts :: Ord a => DFA a -> WordDFA -> Maybe Bool
accepts dfa = go (q0 dfa)
  where
    go q []     = Just $ S.member q (f dfa)
    go q (s:ss)
      | S.member s (sigma dfa) = go (fromJust $ applyTransitionFunction (delta dfa) q s) ss
      | otherwise              = Nothing

even1s = fromJust $ makeDFA [0,1] "01" [((0,'1'),1),((0,'0'),0),((1,'0'),1),((1,'1'),0)] 0 [0]

ends1 = fromJust $ makeDFA [0,1] "01" [((0,'0'),0),((0,'1'),1),((1,'1'),1),((1,'0'),0)] 0 [1]
