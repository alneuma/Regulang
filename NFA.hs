-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

-- Overview:
--
-- Here I chose to implement one of the more general definitions for NFAs. By this I mean:
--
-- - An NFA can have multiple start states i.e. there is a set of start states instead of a single one.
-- - Arrows can be labeled with words over the alphabet of an NFA and not just with symbols.
--   This also allows for epsilon-labeled arrows.

module NFA (NFA, makeNFA, acceptsNFA) where

import qualified Data.Set as S (Set, singleton, findIndex, size, toList, fromList, isSubsetOf, insert, cartesianProduct, empty, map, filter, null, member, union, difference, unions, disjoint, toAscList, foldl')
import qualified Data.Maybe as M (fromJust) 
import qualified Data.Bifunctor as BF (first, second)

type SymbolRL         = Char
type WordRL           = [SymbolRL]
emptyWord             = [] :: WordRL
type AlphabetRL       = S.Set SymbolRL
-- I chose to implement languages as lists instead of sets, as they will often be infinite
-- and sets from Data.Set should only have finite size
type LanguageRL       = [WordRL]
type TransitionRule a = ((a,WordRL),S.Set a)
type Delta a          = S.Set (TransitionRule a)
type States a         = S.Set a

data NFA a = NFA {states :: States a, 
                  sigma :: AlphabetRL,
                  delta :: Delta a,
                  start :: States a,
                  finish :: States a}
                  deriving (Show, Eq)

------------------------------------------------
-- functions for creating and validating NFAs --
------------------------------------------------

-- Convenient way to create an NFA as it takes lists as arguments instead of sets.
--
-- In the type signature I write [SymbolRL] instead of WordRL, because the argument is
-- not to be understood as a word, but as a list of symbols, that will be translated into a
-- an alphabet i.e. a set of symbols.
makeNFA :: (Ord a) => [a] -> [SymbolRL] -> [((a,WordRL),[a])] -> [a] -> [a] -> Maybe (NFA a)
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
-- This can be useful to "simplify" an NFA by "relabeling" it's states with integers, which otherwise
-- often could be of Ord-type values.
-- It also can be used to homogenise the type of different NFAs, which might be necessary for a number
-- of operations.
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
-- the languages recognized by the input NFA's.
-- While it could have been possible to make the return value of type NFA (a,b), instead of
-- starting by converting both NFAs to NFA Int.
-- This would have led to much more complicated code, as I could not just have used the union operation
-- to merge the two NFAs.
unionNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
unionNFA nfa nfb = NFA newStates newSigma newDelta newStart newFinish
  where
    newStates = S.union (states nfaInt) (states nfbInt)
    newSigma  = S.union (sigma  nfaInt) (sigma  nfbInt)
    newDelta  = S.union (delta  nfaInt) (delta  nfbInt)
    newStart  = S.union (start  nfaInt) (start  nfbInt)
    newFinish = S.union (finish nfaInt) (finish nfbInt)
    nfaInt = states2intNFA 0 nfa
    nfbInt = states2intNFA (S.size $ states nfa) nfb

-- removeEmptyEdges
--
-- converts an NFA into an NFA that recognizes the same language, but does not
-- contain any edges labeled with the empty word.
--
removeEmptyEdges :: (Ord a) => NFA a -> NFA a
removeEmptyEdges nfa = nfa {delta = newDelta, start = newStart}
  where
    newStart = processEmptyEdgesNFA (delta nfa) (start nfa)
    newDelta = go (delta nfa) (emptyEdges $ delta nfa)
    go delt eps
      | S.null eps = delt
      | otherwise  = go tmpDelta $ emptyEdges tmpDelta
      where
        tmpDelta = S.difference (S.union delt $ newEdges eps) eps
        newEdges es = S.unions $ S.map newEdges1 eps
        newEdges1 e =   S.map (\((q,s),qs) -> ((fst $ fst e,s),qs))
                      $ S.filter (\t -> S.member (fst $ fst t) (snd e))
                        delt
    emptyEdges = S.filter (\t -> snd (fst t) == emptyWord)

-- -- Takes an NFA and returns the NFA that recognizes the complement of the language recognized
-- -- by the input NFA.
-- -- The complement is the complement relative to all the words over sigma, the NFA's alphabet.
-- -- THIS IS THE BUGGY PART
-- -- PROBLEM: Epsilon-Edges do mess up the idea
-- complementNFA :: (Ord a) => NFA a -> NFA a
-- complementNFA nfa = nfa { finish = S.difference (states nfa) (finish nfa) }
-- 
-- -- Takes two NFAs and returns an NFA that recognizes the intersection of the two languages 
-- -- recognized by the input NFAs.
-- -- DOES NOT WORK CORRECTLY YET
-- intersectionNFA :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
-- intersectionNFA nfa nfb = complementNFA $ unionNFA (complementNFA nfa) (complementNFA nfb)

-----------------------------------------------------
-- functions for checking if an NFA accepts a word --
-----------------------------------------------------

-- acceptsNFA:
-- Verify if a word is accepted by a given NFA
--
-- Returns Nothing if the word does contain symbols which are not in the alphabet of the NFA.
-- Otherwise returns Just True or Just False if the word was accepted or not accepted respectively
--
-- The general strategy is, that the symbols of the word will be examined one by one,
-- each time modifying the set of reached states until no symbols are left anymore.
-- Then it will be checked if there are members of the acceptable states (finish) inside of
-- the set of reached states to see, if the word was accepted.
-- 
-- The main difficulties in this functions implementation is to devise a way by which
-- to properly handle epsilon-labeled edges and edges which are labeled with words
-- of length greater than 1.
--
-- The first difficulty is handled by the function processEmptyEdgesNFA, which,
-- given a set of transition rules (delta), takes a set of states and adds all the
-- states to it, which are reachable from members of this set via epsilon-edges.
--
-- The second difficulty is handled by keeping track of partly resolved arrows,
-- which are kept inside of the "pending" set, which is passed on alongside the set
-- of reached states at each iteration
--
-- The single iterations are performed by the function nextStatesPendingNFA, which alongside the
-- set of transition rules (delta) and the currently examined symbol of the input word, takes the
-- set of currently reached states and a set of only partly resolved arrows (pending) as input and
-- returns a tuple of the new reached states and the new partly resolved arrows.
acceptsNFA :: (Ord a) => NFA a -> WordRL -> Maybe Bool
acceptsNFA nfa word
  | S.isSubsetOf (S.fromList word) (sigma nfa) = Just $ go (start nfa) S.empty word
  | otherwise                                  = Nothing
  where
    go states pending []     = not $ S.disjoint (finish nfa) $ processEmptyEdgesNFA (delta nfa) states
    go states pending (s:ss) = go newStates newPending ss
      where
        (newStates,newPending) = nextStatesPendingNFA (delta nfa) s states pending

nextStatesPendingNFA :: (Ord a) => Delta a -> SymbolRL -> States a -> S.Set (WordRL,S.Set a) -> (States a, S.Set (WordRL,S.Set a))
nextStatesPendingNFA delta symbol states pending = newStatesPending tmpPending
  where
    newStatesPending = go S.empty S.empty . S.toList
      where
        go accStates accPending [] = (accStates,accPending)
        go accStates accPending (p:ps)
          | fst p == emptyWord = go (S.union (snd p) accStates) accPending ps
          | otherwise          = go accStates (S.insert p accPending) ps
    tmpPending = S.union (reducePending pending) $ S.map (\t -> (tail $ snd $ fst t, snd t))
                                                 $ S.filter (\t -> S.member (fst $ fst t) tmpStates)
                                                 $ S.filter (\t -> head (snd $ fst t) == symbol)
                                                 $ S.filter (\t -> snd (fst t) /= emptyWord)
                                                 delta
    tmpStates = processEmptyEdgesNFA delta states
    reducePending = S.map (BF.first tail) . S.filter ((==symbol) . head . fst)

processEmptyEdgesNFA :: (Ord a) => Delta a -> States a -> States a
processEmptyEdgesNFA delta = go
  where
    deltaEpsilonEdges = S.filter ((==emptyWord) . snd . fst) delta
    go stateSetOld
      | S.null stateSetNew = stateSetOld
      | otherwise          = go $ S.union stateSetOld stateSetNew
      where
        -- to avoid infinite loops, we take the difference of a prelimenary new state set and the old state set to determine the new state set.
        -- This way it is made certain, that `go` won't keep iterating when there are no more new states added to the set of states reachable
        -- by ε-edges.
        stateSetNew = S.difference (S.unions $ S.map snd $ S.filter ((`S.member`stateSetOld) . fst . fst) deltaEpsilonEdges) stateSetOld

-----------------------
-- Kleene-operations --
-----------------------

-- returns a List of all possible lists with elements from the input set of length of the input number
kleeneNumber :: (Ord a) => Int -> S.Set a -> [[a]]
kleeneNumber 0 _   = [[]]
kleeneNumber n set = (:) <$> S.toList set <*> kleeneNumber (n-1) set

kleeneStar :: (Ord a) => S.Set a -> [[a]]
kleeneStar set
  | S.null set = [[]]
  | otherwise  = concatMap (`kleeneNumber` set) [0..]

kleenePlus :: (Ord a) => S.Set a -> [[a]]
kleenePlus = tail . kleeneStar

--kleeneStar :: (Ord a) => S.Set a -> [[a]]
--kleeneStar set = concat $ go [[[]]]
--  where
--    go list = list ++ [(:) <$> S.toList set <*> concat list]

--------------------------------
-- testing and comparing NFAs --
--------------------------------

acceptSameWord :: (Ord a) => NFA a -> NFA a -> WordRL -> Bool
acceptSameWord nfa nfb word = acceptsNFA nfa word == acceptsNFA nfb word

acceptSameLanguage :: (Ord a) => Int -> NFA a -> NFA a -> LanguageRL -> Bool
acceptSameLanguage n nfa nfb = all (acceptSameWord nfa nfb) . take n

acceptsLanguageVector :: (Ord a) => Int -> NFA a -> LanguageRL -> [Maybe Bool]
acceptsLanguageVector n nfa = take n . map (acceptsNFA nfa)

------------------------------
-- example and testing NFAs --
------------------------------

-- zeros2div3div
-- an NFA that has an alphabet that consists only of '0' and accepts any number of '0's that is divisible by 2 or 3
zeros2div3div = M.fromJust $ makeNFA [0,1,2,3,4,5] ['0'] [((0,emptyWord),[1,3]),((1,['0']),[2]),((2,['0']),[1]),((3,['0']),[4]),((4,['0']),[5]),((5,['0']),[3])] [0] [1,3]

zeros2div3divInt = states2intNFA 0 zeros2div3div

-- notZeros2div3div = complementNFA zeros2div3div

even1s = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[1]),((0,['1']),[1]),((1,['1']),[0])] [0] [0]

endsWith1 = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[0]),((0,['1']),[1]),((1,['1']),[1])] [0] [1]

abc = M.fromJust $ makeNFA [0,1] ['a','b','c'] [((0,"abc"),[1])] [0] [1]

unionEndsWith1even1s = unionNFA endsWith1 even1s

-- notEndsWith1 = complementNFA endsWith1

-- notEven1s = complementNFA even1s

-- intersectionEndsWith1even1s = intersectionNFA endsWith1 even1s

zeros2div3divNoEps = removeEmptyEdges zeros2div3div

testOK = acceptSameLanguage 20 zeros2div3div zeros2div3divNoEps $ kleeneStar $ S.fromList ['0']
