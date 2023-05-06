-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module NFA (NFA,makeNFA,accepts,getReachableStates,getCoReachableStates,getTrimStates,removeStates,simplify,toIntNFA,union,replaceEmptyEdges,replaceWordEdges,replaceSetValuedEdges,kleeneNumber,kleeneStar,kleenePlus) where

import qualified Data.Maybe     as M  (fromJust) 
import qualified Data.Bifunctor as BF (first, second)
import qualified Text.Printf    as P  (printf)
import qualified Data.Set       as S  (Set, singleton, findIndex, size, toList, fromList, isSubsetOf, insert, cartesianProduct, empty, map, filter, null, member, union, intersection, difference, unions, disjoint, toAscList, foldl')

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

-- nondeterministic finita automation
data NFA a = NFA {states :: States a, 
                  sigma  :: AlphabetRL,
                  delta  :: Delta a,
                  start  :: States a,
                  finish :: States a}
                  deriving Eq

-- regular expressions
data RegEx = RE SymbolRL
           | Empty
           | Epsilon
           | Concat RegEx RegEx
           | Kleene RegEx
           | Union RegEx

-- grammar
data Grammar = G {variables   :: AlphabetRL,
                  terminals   :: AlphabetRL,
                  rules       :: S.Set (WordRL,WordRL),
                  startSymbol :: SymbolRL}

class RegularLanguage a where
  toList      :: a -> LanguageRL
  toNFA       :: a -> NFA Int
  toRegEx     :: a -> RegEx
  toGrammar   :: a -> Grammar

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

getTrimStates :: (Ord a) => NFA a -> States a
getTrimStates nfa = S.intersection (getReachableStates nfa) (getCoReachableStates nfa)

getCoReachableStates :: (Ord a) => NFA a -> States a
getCoReachableStates nfa = extendStatesByRule nfa followEdgesBackwards $ finish nfa
  where
    followEdgesBackwards states = S.difference newStates states
      where
        newStates = S.map (fst . fst)
                  $ S.filter (not . S.null . (`S.intersection` states) . snd)
                  $ delta nfa

getReachableStates :: (Ord a) => NFA a -> States a
getReachableStates nfa = extendStatesByRule nfa followEdges $ start nfa
  where
    followEdges states = S.difference newStates states
      where
        newStates = S.unions
                  $ S.map snd
                  $ S.filter ((`S.member` states) . fst . fst)
                  $ delta nfa

----------------------------------------------------
-- operations for creating new NFAs from old ones --
----------------------------------------------------

-- Takes and NFA and converts it into an equivalent NFA, which has Int type states starting
-- with smallest
-- This can be useful to "simplify" an NFA by "relabeling" it's states with integers, which otherwise
-- often could be of Ord-type values.
-- It also can be used to homogenise the type of different NFAs, which might be necessary for a number
-- of operations.
toIntNFA :: (Ord a) => Int -> NFA a -> NFA Int
toIntNFA smallest nfa = NFA newStates (sigma nfa) newDelta newStart newFinish
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
union :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
union nfa nfb = NFA newStates newSigma newDelta newStart newFinish
  where
    newStates = S.union (states nfaInt) (states nfbInt)
    newSigma  = S.union (sigma  nfaInt) (sigma  nfbInt)
    newDelta  = S.union (delta  nfaInt) (delta  nfbInt)
    newStart  = S.union (start  nfaInt) (start  nfbInt)
    newFinish = S.union (finish nfaInt) (finish nfbInt)
    nfaInt = toIntNFA 0 nfa
    nfbInt = toIntNFA (S.size $ states nfa) nfb

-- replaceEmptyEdges
--
-- converts an NFA into an NFA that recognizes the same language, but does not
-- contain any edges labeled with the empty word.
--
replaceEmptyEdges :: (Ord a) => NFA a -> NFA a
replaceEmptyEdges nfa = nfa {delta = newDelta, start = newStart}
  where
    newStart   = processEmptyEdges (delta nfa) (start nfa)
    newDelta   = go (delta nfa) (emptyEdges $ delta nfa)
    emptyEdges = S.filter (\t -> snd (fst t) == emptyWord)
    go delt eps
      | S.null eps = delt
      | otherwise  = go tmpDelta $ emptyEdges tmpDelta
      where
        tmpDelta = S.difference (S.union delt $ newEdges eps) eps
        newEdges es = S.unions $ S.map newEdges1 eps
        newEdges1 e = S.map (\((q,s),qs) -> ((fst $ fst e,s),qs))
                    $ S.filter (\t -> S.member (fst $ fst t) (snd e)) delt

-- replaceWordEdges
--
-- Replaces all the edges of an NFA that are labeled with words of length > 1, 
-- in a way, that produces an equivalent NFA, that does not have such edges.
--
-- Before the the rest of the calculation, we convert the NFA into an NFA
-- with Int states, as this makes it easier to handle the creation of new states,
-- which is necessary for this function.
--
replaceWordEdges :: (Ord a) => NFA a -> NFA Int
replaceWordEdges nfa = tmpNFA {states = newStates, delta = newDelta}
  where
    tmpNFA               = toIntNFA 0 nfa
    tmpDelta             = S.difference (delta tmpNFA) wordEdges
    wordEdges            = S.filter ((1<) . length . snd . fst) $ delta tmpNFA
    (newStates,newDelta) = go (S.size $ states tmpNFA) (states tmpNFA) tmpDelta $ S.toList wordEdges
    go count states delta []     = (states,delta)
    go count states delta (e:es) = go count' states' delta' es
      where
        count'      = (+) count $ length $ snd $ fst e
        states'     = S.union states $ S.fromList addedStates
        delta'      = S.union delta $ S.fromList addedEdges
        addedStates = [count..count'-2]
        addedEdges  = zip (zip (fst (fst e) : addedStates) (map (:[]) $ snd $ fst e))
                          (map S.singleton addedStates ++ [snd e])

-- replaceSetValuedEdges
--
-- Makes an equivalent NFA, that is described only by edges pointing to singleton sets,
-- not to sets of size > 1.
--
replaceSetValuedEdges :: (Ord a) => NFA a -> NFA a
replaceSetValuedEdges nfa = nfa {delta = newDelta}
  where
    wordDelta  = S.filter ((>1) . S.size . snd) $ delta nfa
    addedDelta = S.unions $ S.map split1 wordDelta
    split1 ((q,w),qs) = S.map (\r -> ((q,w),S.singleton r)) qs
    newDelta          = S.union addedDelta $ S.difference (delta nfa) wordDelta

removeStates :: (Ord a) => NFA a -> States a -> NFA a
removeStates nfa statesToRemove = nfa {states = newStates,
                                       delta  = newDelta,
                                       start  = newStart,
                                       finish = newFinish}
  where
    removeThemFrom = (`S.difference` statesToRemove)
    newStates = removeThemFrom $ states nfa
    newStart  = removeThemFrom $ start  nfa
    newFinish = removeThemFrom $ finish nfa
    newDelta  = S.map (BF.second removeThemFrom)
              $ S.filter ((`S.member` newStates) . fst . fst)
              $ delta nfa

simplify :: (Ord a) => NFA a -> NFA a
simplify nfa = removeStates nfa (S.difference (states nfa) $ getTrimStates nfa)

-- private function, used for a number of other functions
-- 
-- Given an NFA, a set of states and a rule that specifies, how to add states to an already existing set of states
-- The input set of states will be iteratively expanded by this rule until no more new states get added.
--
-- Two important notes:
-- - To avoid an infinite repetition it is vital, that the rule is composed in a way, that
--   That it's input and output set don't share any members.
-- - It should be kept in mind, that for each iteration only the set of newly added states
--   will be the input for the rule of the next iteration. The merging of all the so created
--   sets does happen later.
--   This is because it is assumed, that a state does not have anything further to contribute after
--   it was used once as element of the input state for the rule.
--   Make sure, that the rule is written in a way that what states are newly derived from it
--   does only depend on each state of the input set individually.
extendStatesByRule :: (Ord a) => NFA a -> (States a -> States a) -> States a -> States a
extendStatesByRule nfa rule = S.unions . takeWhile (not . S.null) . applyRepeatedly rule
  where
    applyRepeatedly f x = x : applyRepeatedly f (f x)

-----------------------------------------------------
-- functions for checking if an NFA accepts a word --
-----------------------------------------------------

-- accepts:
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
-- The first difficulty is handled by the function processEmptyEdges, which,
-- given a set of transition rules (delta), takes a set of states and adds all the
-- states to it, which are reachable from members of this set via epsilon-edges.
--
-- The second difficulty is handled by keeping track of partly resolved arrows,
-- which are kept inside of the "pending" set, which is passed on alongside the set
-- of reached states at each iteration
--
-- The single iterations are performed by the function nextStatesPending, which alongside the
-- set of transition rules (delta) and the currently examined symbol of the input word, takes the
-- set of currently reached states and a set of only partly resolved arrows (pending) as input and
-- returns a tuple of the new reached states and the new partly resolved arrows.
accepts :: (Ord a) => NFA a -> WordRL -> Maybe Bool
accepts nfa word
  | S.isSubsetOf (S.fromList word) (sigma nfa) = Just $ go (start nfa) S.empty word
  | otherwise                                  = Nothing
  where
    go states pending []     = not $ S.disjoint (finish nfa) $ processEmptyEdges (delta nfa) states
    go states pending (s:ss) = go newStates newPending ss
      where
        (newStates,newPending) = nextStatesPending (delta nfa) s states pending

nextStatesPending :: (Ord a) => Delta a -> SymbolRL -> States a -> S.Set (WordRL,S.Set a) -> (States a, S.Set (WordRL,S.Set a))
nextStatesPending delta symbol states pending = newStatesPending tmpPending
  where
    newStatesPending = go S.empty S.empty . S.toList
      where
        go accStates accPending [] = (accStates,accPending)
        go accStates accPending (p:ps)
          | fst p == emptyWord = go (S.union (snd p) accStates) accPending ps
          | otherwise          = go accStates (S.insert p accPending) ps
    tmpPending = S.union (reducePending pending)
               $ S.map (\t -> (tail $ snd $ fst t, snd t))
               $ S.filter (\t -> S.member (fst $ fst t) tmpStates)
               $ S.filter (\t -> head (snd $ fst t) == symbol)
               $ S.filter (\t -> snd (fst t) /= emptyWord) delta
    tmpStates = processEmptyEdges delta states
    reducePending = S.map (BF.first tail) . S.filter ((==symbol) . head . fst)

processEmptyEdges :: (Ord a) => Delta a -> States a -> States a
processEmptyEdges delta = go
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
acceptSameWord nfa nfb word = accepts nfa word == accepts nfb word

recognizeSameLanguage :: (Ord a) => Int -> NFA a -> NFA a -> LanguageRL -> Bool
recognizeSameLanguage n nfa nfb = all (acceptSameWord nfa nfb) . take n

recognizeLanguageVector :: (Ord a) => Int -> NFA a -> LanguageRL -> [Maybe Bool]
recognizeLanguageVector n nfa = take n . map (accepts nfa)

---------------------
-- displaying NFAs --
---------------------

instance (Show a) => Show (NFA a) where
    -- show :: (Show a) => NFA a -> String
    show nfa = "States:\t" ++ showSet   (states nfa) ++ "\n" ++
               "Sigma:\t"  ++ showSet   (sigma  nfa) ++ "\n" ++
               "Delta:\t"  ++ showDelta (delta  nfa) ++
               "Start:\t"  ++ showSet   (start  nfa) ++ "\n" ++
               "Finish:\t" ++ showSet   (finish nfa)
      where
        showDelta delta = showEdge (head elementsDelta) ++ concatMap (("\t" ++) . showEdge) (tail elementsDelta)
          where elementsDelta = S.toAscList delta
        showEdge ((q,s),qs) = "(" ++ show q ++ ", " ++ show s ++ ")\t-> " ++ showSet qs ++ "\n"

showSet :: (Show a) => S.Set a -> String
showSet set
  | S.null set = "{}"
  | otherwise  = "{" ++ show (head elements) ++ concatMap (("," ++) . show) (tail elements) ++ "}"
  where
    elements = S.toAscList set

------------------------------
-- example and testing NFAs --
------------------------------

-- zeros2div3div
-- an NFA that has an alphabet that consists only of '0' and accepts any number of '0's that is divisible by 2 or 3
zeros2div3div = M.fromJust $ makeNFA [0,1,2,3,4,5] ['0'] [((0,emptyWord),[1,3]),((1,['0']),[2]),((2,['0']),[1]),((3,['0']),[4]),((4,['0']),[5]),((5,['0']),[3])] [0] [1,3]

zeros2div3divInt = toIntNFA 0 zeros2div3div

-- notZeros2div3div = complementNFA zeros2div3div

even1s = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[1]),((0,['1']),[1]),((1,['1']),[0])] [0] [0]

endsWith1 = M.fromJust $ makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[0]),((0,['1']),[1]),((1,['1']),[1])] [0] [1]

abc = M.fromJust $ makeNFA [0,1] ['a','b','c'] [((0,"abc"),[1])] [0] [1]

abcNoWords = replaceWordEdges abc

unionEndsWith1even1s = endsWith1 `union` even1s

-- notEndsWith1 = complementNFA endsWith1

-- notEven1s = complementNFA even1s

-- intersectionEndsWith1even1s = intersectionNFA endsWith1 even1s

zeros2div3divNoEps = replaceEmptyEdges zeros2div3div

testReplaceEmptyEdges = recognizeSameLanguage 20 zeros2div3div zeros2div3divNoEps $ kleeneStar $ S.fromList ['0']

testTrimNFA = M.fromJust $ makeNFA [0..10] "0" (zip (zip [1,2,3,5,7,8,9,10] $ repeat ['0']) [[2,5],[3],[4],[6],[8],[4],[10],[9]]) [1] [4]
