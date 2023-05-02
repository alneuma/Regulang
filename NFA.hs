-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module NFA (NFA, makeNFA, acceptsNFA) where

import qualified Data.Set as S (Set, singleton, findIndex, size, toList, fromList, isSubsetOf, insert, cartesianProduct, empty, map, filter, null, member, union, difference, unions, disjoint, toAscList)
import qualified Data.Maybe as M (fromJust) 
import qualified Data.Bifunctor as BF (second)

-- As for NFAs we want to allow for ε-labeled arrows we need a value of type Symbol to represent
-- the empty string. Although from a Haskell perspective it would make most sense to implement the Symbol type
-- as String I here chose to use Char as the default.
-- The ratio is that like this words will be Strings instead of lists of Strings, which will make it more comfortable
-- to interact with the module through ghci.
-- To change from Symbol = Char to Symbol = String, simply comment out the next two lines after the current comment
-- and uncomment the two lines following those. This should not effect program behavior
type Symbol           = Char
sempty                = '\0' :: Symbol
-- type Symbol           = String
-- sempty                = "" :: Symbol
type Alphabet         = S.Set Symbol
type Language         = S.Set [Symbol]
type TransitionRule a = ((a,Symbol),S.Set a)
type Delta a          = S.Set (TransitionRule a)
type States a         = S.Set a


data NFA a = NFA {states :: States a, 
                  sigma :: Alphabet,
                  delta :: Delta a,
                  start :: States a,
                  finish :: States a}
                  deriving (Show, Eq)

------------------------------------------------
-- functions for creating and validating NFAs --
------------------------------------------------

-- Convinient way to create an NFA as it takes lists as arguments instead of sets.
-- The Symbol representing an empty word should be passed as sempty.
makeNFA :: (Ord a) => [a] -> [Symbol] -> [((a,Symbol),[a])] -> [a] -> [a] -> Maybe (NFA a)
makeNFA states sigma delta start finish
  | validNFA newNFA = Just newNFA
  | otherwise       = Nothing
  where
    newNFA = NFA newStates newSigma newDelta newStart newFinish
    newStates = S.fromList states
    newSigma  = S.fromList sigma
    newDelta  = S.fromList $ map (BF.second S.fromList) delta
    newStart  = S.fromList start
    newFinish = S.fromList finish

-- Checks if an NFA is defined correctly.
-- Returns False if any of the following holds
-- - start is no subset of states
-- - finish is no subset of states
-- - not all left entries of delta have their left entries in states or their right entry as an empty
--   list or with a single argument, that is in sigma
-- - the set of the right entries of delta is not a subset of states
-- Otherwise returns True.
--
-- Needs general rewrite also to handle ε-lableded arrows
validNFA :: (Ord a) => NFA a -> Bool
validNFA inputFA = True
--                    S.isSubsetOf (start  inputFA) (states inputFA) &&
--                    S.isSubsetOf (finish inputFA) (states inputFA) &&
--                    S.isSubsetOf deltaLeft (S.cartesianProduct (states inputFA) (sigma inputFA)) -- &&
--                    --S.empty (S.filter (not . (`S.isSubsetOf` (states inputFA))) deltaRight)
--   where
--     deltaLeft  = S.map ((\(arg,[val]) -> (arg,val)) . fst) (delta inputFA)
--     deltaRight = S.map snd (delta inputFA)

----------------------------------------------------
-- operations for creating new NFAs from old ones --
----------------------------------------------------

-- Takes and NFA and converts it into an equivalent NFA, which has Int type states starting
-- with smallest
states2intNFA :: (Ord a) => Int -> NFA a -> NFA Int
states2intNFA smallest nfa = NFA newStates (sigma nfa) newDelta newStart newFinish
  where
    newStates = S.fromList $ zipWith const [smallest..] $ S.toAscList $ states nfa
    newDelta  = S.map transitionRule2int $ delta nfa
    newStart  = S.map state2int $ start nfa
    newFinish = S.map state2int $ finish nfa
    state2int state = (+smallest) $ S.findIndex state $ states nfa
    transitionRule2int ((stateArg,symbol),stateSetVal) = ((state2int stateArg,symbol),S.map state2int stateSetVal)

-- Takes two NFAs and returns an NFA that recognizes the language which is the union of
-- the languages recognized by the input NFA's
unionNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
unionNFA nfa nfb = NFA newStates newSigma newDelta newStart newFinish
  where
    newStates = S.insert 0 $ S.union (states nfaInt) (states nfbInt)
    newSigma  = S.union (sigma  nfaInt) (sigma  nfbInt)
    newDelta  = S.unions [newEdges, delta  nfaInt, delta  nfbInt]
    newStart  = S.singleton 0
    newFinish = S.union (finish nfaInt) (finish nfbInt)
    newEdges  = S.union (S.map makeEpsilonEdgeFrom0 $ start nfaInt)
                        (S.map makeEpsilonEdgeFrom0 $ start nfbInt)
    nfaInt = states2intNFA 1 nfa
    nfbInt = states2intNFA ((+1) $ S.size $ states nfa) nfb
    makeEpsilonEdgeFrom0 state = ((0,sempty),S.singleton state)

-- Takes an NFA and returns the NFA that recognizes the complement of the language recognized
-- by the input NFA.
-- The complement is the complement relative to all the words over sigma, the NFA's alphabet.
-- THIS IS THE BUGGY PART
-- PROBLEM: Epsilon-Edges do mess up the idea
complementNFA :: (Ord a) => NFA a -> NFA a
complementNFA nfa = nfa { finish = S.difference (states nfa) (finish nfa) }

-- Takes two NFAs and returns an NFA that recognizes the intersection of the two languages 
-- recognized by the input NFAs.
-- DOES NOT WORK CORRECTLY YET
intersectionNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
intersectionNFA nfa nfb = complementNFA $ unionNFA (complementNFA nfa) (complementNFA nfb)

-----------------------------------------------------
-- functions for checking if an NFA accepts a word --
-----------------------------------------------------

-- returns Nothing when any Symbol of the input word is not in the NFA's alphabet (sigma).
-- returns Just True when the NFA accepts the input word
-- returns Just False otherwise
acceptsNFA :: (Ord a) => NFA a -> [Symbol] -> Maybe Bool
acceptsNFA nfa = go (start nfa)
  where
    go stateSet [] = Just $ not $ S.disjoint (processEpsilonEdgesNFA (delta nfa) stateSet) (finish nfa) 
    go stateSet (s:ss)
      | s `S.member` sigma nfa = go (nextStateSetNFA (delta nfa) s stateSet) ss
      | otherwise              = Nothing

-- Processing the next symbol of a word consists in two steps:
--
-- First all the states reachable by following epsilon edges are added to the current state set.
-- Then the new state set is determined by following all the edges from states of the current state set that are
-- tagged with the symbol.
--
-- processEpsilonEdgesNFA performs the first step.
-- processNormalEdgesNFA performs the second step.
--
-- no error checking is needed, as the calling function (acceptsNFA) checks weather a symbol is part of the allowed alphabet
nextStateSetNFA :: (Ord a) => Delta a -> Symbol -> States a -> States a
nextStateSetNFA delta symbol = processNormalEdgesNFA delta symbol . processEpsilonEdgesNFA delta

processEpsilonEdgesNFA :: (Ord a) => Delta a -> States a -> States a
processEpsilonEdgesNFA delta = go
  where
    deltaEpsilonEdges = S.filter ((==sempty) . snd . fst) delta
    go stateSetOld
      | S.null stateSetNew = stateSetOld
      | otherwise          = go $ S.union stateSetOld stateSetNew
      where
        -- to avoid infinite loops, we take the difference of a prelimenary new state set and the old state set to determine the new state set.
        -- This way it is made certain, that `go` won't keep iterating when there are no more new states added to the set of states reachable
        -- by ε-edges.
        stateSetNew = S.difference (S.unions $ S.map snd $ S.filter ((`S.member`stateSetOld) . fst . fst) deltaEpsilonEdges) stateSetOld

processNormalEdgesNFA :: (Ord a) => Delta a -> Symbol -> States a -> States a
processNormalEdgesNFA delta symbol states = S.unions $ S.map snd $ S.filter ((`S.member`states) . fst . fst) $ S.filter ((==symbol) . snd . fst) delta

------------------
-- example NFAs --
------------------

-- zeros2div3div
-- an NFA that has an alphabet that consists only of '0' and accepts any number of '0's that is divisible by 2 or 3
zeros2div3div = M.fromJust $ makeNFA [0,1,2,3,4,5] ['0'] [((0,sempty),[1,3]),((1,'0'),[2]),((2,'0'),[1]),((3,'0'),[4]),((4,'0'),[5]),((5,'0'),[3])] [0] [1,3]

zeros2div3divInt = states2intNFA 0 zeros2div3div

notZeros2div3div = complementNFA zeros2div3div

even1s = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,'0'),[0]),((1,'0'),[1]),((0,'1'),[1]),((1,'1'),[0])] [0] [0]

endsWith1 = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,'0'),[0]),((1,'0'),[0]),((0,'1'),[1]),((1,'1'),[1])] [0] [1]

unionEndsWith1even1s = unionNFA endsWith1 even1s

notEndsWith1 = complementNFA endsWith1

notEven1s = complementNFA even1s

intersectionEndsWith1even1s = intersectionNFA endsWith1 even1s
