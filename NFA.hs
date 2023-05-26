-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module NFA (renameStates,replaceEmptyEdges,replaceWordEdges,eqClassesDFA) where

{-# LANGUAGE TupleSections #-}

import qualified Data.Bifunctor as BF (first,second)
import qualified Data.Maybe     as M  (isJust,fromJust,isNothing) 
import qualified Data.Set       as S
import TypesRL
import ELabel
import ShowRL
import RegLang
import SetExtensionRL
import SymbolRL

instance (Show a, ShowRL b) => ShowRL (NFA a b) where
    showRL nfa = "States:\t" ++ showRL    (states nfa) ++ "\n" ++
                 "Sigma:\t"  ++ showRL    (sigma  nfa) ++ "\n" ++
                 "Delta:\t"  ++ deltaString            ++
                 "Start:\t"  ++ showRL    (start  nfa) ++ "\n" ++
                 "Finish:\t" ++ showRL    (finish nfa)
      where
        deltaString = showRL (head elementsDelta) ++ concatMap (("\t" ++) . showRL) (tail elementsDelta)
        elementsDelta = S.toAscList $ delta nfa

instance (Integral a, ELabel b) => RegLang (NFA a b) where
  toList :: (Integral a, ELabel b) => NFA a b -> [WordRL]
  toList nfa = filter ((== Just True) . accepts nfa) (kleeneStar $ sigma nfa)

  toDFA :: (Integral a, ELabel b, Integral c) => NFA a b -> NFA c SymbolRL
  toDFA nfa = customRename 0 $ NFA tmpStates tmpSigma tmpDelta tmpStart tmpFinish
    where
      customRename smallest nfa = NFA newStates (sigma nfa) newDelta newStart newFinish
        where
          newStates = S.fromList $ zipWith const [smallest..] $ S.toAscList $ states nfa
          newDelta  = S.map transitionRule2int $ delta nfa
          newStart  = S.map state2int $ start nfa
          newFinish = S.map state2int $ finish nfa
          state2int state = (+smallest) $ fromInteger $ toInteger $ S.findIndex state $ states nfa
          transitionRule2int ((stateArg,symbol),stateSetVal) = ((state2int stateArg,symbol),S.map state2int stateSetVal)
      tmpStates = S.map fst dfaAsSet
      tmpSigma  = sigma tmpNFA
      tmpDelta  = S.unions
                $ S.map (\(q,e) -> S.map (\(l,t) -> ((q,l),S.singleton t)) e) dfaAsSet
      tmpStart  = S.singleton $ start tmpNFA
      tmpFinish = S.filter (not . S.null . S.intersection (finish tmpNFA))
                $ S.map fst dfaAsSet
      listSigma = S.toList $ sigma nfa
      tmp0NFA = replaceEmptyEdges $ replaceWordEdges $ simplify nfa
      tmpNFA  = tmp0NFA {delta = S.map (\((q,l),rs) -> ((q,M.fromJust $ singletonToSymbol l),rs))
                               $ delta tmp0NFA}
      dfaAsSet = go S.empty (S.singleton $ start tmpNFA)
      go processed unprocessed
        | S.null unprocessed = processed
        | otherwise          = go (S.insert newRules processed)
                                  ((newStates `S.difference` S.map fst processed)
                                    `S.union` newUnprocessed)
        where
          (setToProcess,newUnprocessed) = S.deleteFindMin unprocessed
          (newRules,newStates) = go2 setToProcess
          go2 set = ((set,edges),stateSets)
            where
              edges = S.map (\sy -> (sy,followEdges set sy)) $ sigma tmpNFA
              stateSets = S.map snd edges
              followEdges s sym = S.unions
                                $ S.map getTo
                                $ S.filter ((==sym) . getLabel)
                                $ S.filter ((`S.member` s) . getFrom)
                                $ delta tmpNFA

  union :: (Integral a, ELabel b) => NFA a b -> NFA a b -> NFA a b
  union nfa nfb = NFA newStates newSigma newDelta newStart newFinish
    where
      newStates = S.union (states nfaInt) (states nfbInt)
      newSigma  = S.union (sigma  nfaInt) (sigma  nfbInt)
      newDelta  = S.union (delta  nfaInt) (delta  nfbInt)
      newStart  = S.union (start  nfaInt) (start  nfbInt)
      newFinish = S.union (finish nfaInt) (finish nfbInt)
      nfaInt = renameStates 0 nfa
      nfbInt = renameStates (fromInteger $ toInteger $ S.size $ states nfa) nfb


  accepts :: (Integral a, ELabel b) => NFA a b -> WordRL -> Maybe Bool
  -- First a list is created, that contains all the start states, attached with the input word to them.
  -- Each of these (word,state) tuples, can be understood as one moment during the parallel nondeterministic
  -- execution of the NFA.
  -- Then this list is iteratively traversed.
  -- During each iteration each of these "moments" disappears or creates a number of new "moments",
  -- depending on which other states are reachable by what edges.
  -- When a "moment" is reached in which the word part is empty, it is returned True,
  -- if the associated state is one of the accepted states of the NFA. Otherwise
  -- the "moment" gets dropped from the list and won't be considered anymore in the future.
  accepts nfa word
    | not $ S.isSubsetOf (S.fromList word) (sigma nfa) = Nothing
    | otherwise = Just 
                $ execute S.empty
                $ map (word,)
                $ S.toList
                $ start nfa
    where
      -- execute handles the iterative traversal of the list of "moments"
      execute acc []
        | S.null acc              = False
        | otherwise               = execute S.empty $ S.toList acc
      execute acc (([],q):qs)
        | S.member q $ finish nfa = True
        | otherwise               = execute acc qs
      execute acc ((w,q):qs)  = execute (S.union (followStates (w,q)) acc) qs
      followStates            = S.unions . S.map follow1State . followEmptyEdges
      -- follow1State returns all the states reachable by traversing one (non-empty)
      -- edge from q with w.
      -- I separate following empty-labeled edges and normal edges, to make sure
      -- that after an empty-labeled edge got followed, it will not be followed again
      -- from the same "moment", but only during a "moment" in the future, where the
      -- moment's is a different one.
      -- Without this precaution, there are scenarios, where with certain NFA's infinite
      -- loops of empty-edge-following could appear.
      follow1State (w,q)      = S.unions
                              $ S.map (follow1Edge w)
                              $ S.filter (M.isJust . flip containedPart w . getLabel)
                              $ S.filter (not . isEmptyLabel . getLabel)
                              $ S.filter ((==q) . getFrom)
                              $ delta nfa
      -- follow1Edge takes a word and an edge and returns all the "moments",
      -- we could have after traversing this edge with the word.
      -- For an ELabel of type SymbolRL or WordRL the output is just (w,) mapped to
      -- the set of the destination states of the edge.
      -- For an ELabel of type RegEx, there are more possibilities, as one regular
      -- expression can accepts different words, which also means, that the associated
      -- edge could be traversed by different words, which would also lead to a more
      -- diverse set of resulting "moments"
      follow1Edge w edge      = S.cartesianProduct
                                (M.fromJust $ leftAfterTraversal (getLabel edge) w)
                                (getTo edge)
      -- addEmptyReachable creates a set with all the (w,q') tuples, where q' is
      -- reachable from q, by only traversing empty-labeled edges.
      followEmptyEdges (w,q)  = S.map (w,)
                              $ getEmptyReachableStates nfa
                              $ S.singleton q

  addWord :: NFA a b -> WordRL -> NFA a b
  addWord nfa = union nfa . word2NFA

  concatenate :: (Integral a, ELabel b) => NFA a b -> NFA a b -> NFA a b
  concatenate nfa nfb = S.foldl (\acc (q,q') -> insertEmptyEdge acc q q') tmpNFA
                 $ S.cartesianProduct (finish nfaInt) (start nfbInt)
    where
      tmpNFA = NFA tmpStates tmpSigma tmpDelta tmpStart tmpFinish
      tmpStates = S.union (states nfaInt) (states nfbInt)
      tmpSigma  = S.union (sigma  nfaInt) (sigma  nfbInt)
      tmpDelta  = S.union (delta  nfaInt) (delta  nfbInt)
      tmpStart  = start  nfaInt
      tmpFinish = finish nfbInt
      nfaInt = renameStates 0 nfa
      nfbInt = renameStates (fromInteger $ toInteger $ S.size $ states nfa) nfb

  kleene :: (Integral a, ELabel b) => NFA a b -> NFA a b
  kleene nfa = S.foldl (\acc (q,q') -> insertEmptyEdge acc q q') newNFA
             $ S.cartesianProduct (finish newNFA) (start intNFA)             
    where
      -- it is not necessary to explicitly add 0 to the states of intNFA,
      -- as this will be automatically done by the insertEmptyEdge function
      newNFA    = intNFA {start  = newStart,
                          finish = newFinish}
      newStart  = S.singleton 0
      newFinish = S.insert 0 $ finish intNFA
      intNFA    = renameStates 1 nfa

  simplify :: (Ord a, Ord b) => NFA a b -> NFA a b
  simplify nfa = removeStates nfa (S.difference (states nfa) $ getTrimStates nfa)



-- Takes a DFA and groups it's states into equivalence-classes.
-- Can be used as the first step in creating a minimal automaton
eqClassesDFA :: (Integral a) => NFA a SymbolRL -> [[a]]
eqClassesDFA dfa
  | S.null (finish dfa)     = [S.toList $ states dfa]
  | S.null finishComplement = [S.toList $ states dfa]
  | otherwise               = findEqClasses 2
                                            dfa
                                            [S.toList finishComplement, S.toList $ finish dfa]
                                            (map (:[]) $ S.toList $ sigma dfa)
    where
      finishComplement = states dfa `S.difference` finish dfa

findEqClasses :: (Integral a) => Int -> NFA a SymbolRL -> [[a]] -> [WordRL] -> [[a]]
findEqClasses numClasses dfa classes words
  | numClasses == newNumClasses = newClasses
  | otherwise                   = findEqClasses newNumClasses
                                                dfa
                                                newClasses
                                                ((:) <$> S.toList (sigma dfa) <*> words)
  where
    newNumClasses = length newClasses
    tmpClasses    = addPatterns dfa classes words
    newClasses    = refineAllEqClasses tmpClasses

refineAllEqClasses :: (Integral a) => [[(a,[Bool])]] -> [[a]]
refineAllEqClasses [] = []
refineAllEqClasses classes@(xs:xss)
  | null xs              = []
  | null (snd $ head xs) = map (map fst) classes
  | otherwise            = refineAllEqClasses $ concatMap refineEqClass classes

-- takes a list of states with added "word-acceptance-vectors" and groups them
-- according to the first entry of this vector removes all the first entries
-- of these vectors
refineEqClass :: (Integral a) => [(a,[Bool])] -> [[(a,[Bool])]]
refineEqClass []                 = []
refineEqClass eqClass@((_,[]):_) = [eqClass]
refineEqClass eqClass = go [] [] eqClass
  where
    go true []    [] = [true]
    go []   false [] = [false]
    go true false [] = [true,false]
    go true false ((q,b:bs):xs)
      | b         = go ((q,bs):true) false xs
      | otherwise = go true ((q,bs):false) xs

-- adds to each state in the list of state lists a list denoting which words are accepted or not.
addPatterns :: (Integral a) => NFA a SymbolRL -> [[a]] -> [WordRL] -> [[(a,[Bool])]]
addPatterns dfa classes words = map (map (getPattern dfa words)) classes
  where
    getPattern d w s = (s,pattern)
      where
        pattern = map (M.fromJust . stateAccepts d s) w

-- checks a an NFA accepts a word starting from a specified state
stateAccepts :: (Integral a, ELabel b) => NFA a b -> a -> WordRL -> Maybe Bool
stateAccepts nfa state word
  | S.member state (states nfa) = accepts modifiedNFA word
  | otherwise                   = Nothing
  where
    modifiedNFA = nfa {start = S.singleton state}



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
validNFA :: (Ord a, ELabel b) => NFA a b -> Bool
validNFA nfa = not (S.null $ start nfa) &&
               S.isSubsetOf allStates (states nfa) &&
               S.isSubsetOf deltaAlphabet (sigma nfa)
  where
    allStates     = S.unions [start nfa, finish nfa, deltaFrom, deltaTo]
    deltaFrom     = S.map (fst . fst) (delta nfa)
    deltaTo       = S.unions
                  $ S.map snd (delta nfa)
    deltaAlphabet = S.unions
                  $ S.map (getAlphabet . snd . fst) (delta nfa)

validDFA :: (Ord a, ELabel b) => NFA a b -> Bool
validDFA nfa = validNFA nfa &&
               all isSingletonLabel (S.toList labels) &&
               all ((==1) . S.size) deltaRange &&
               deltaDomain == S.cartesianProduct (states nfa) (sigma nfa)
  where
    labels      = S.map (snd . fst) $ delta nfa
    deltaRange  = S.map snd $ delta nfa
    -- rework deltaDomain with set-functions
    deltaDomain = S.map (BF.second (head . S.toList . getAlphabet))
                $ S.map fst $ delta nfa

getTrimStates :: (Ord a) => NFA a b -> States a
getTrimStates nfa = S.intersection (getReachableStates nfa) (getCoReachableStates nfa)

getCoReachableStates :: (Ord a) => NFA a b -> States a
getCoReachableStates nfa = extendStatesByRule nfa followEdgesBackwards $ finish nfa
  where
    followEdgesBackwards states = S.difference newStates states
      where
        newStates = S.map (fst . fst)
                  $ S.filter (not . S.null . (`S.intersection` states) . snd)
                  $ delta nfa

getReachableStates :: (Ord a) => NFA a b -> States a
getReachableStates nfa = extendStatesByRule nfa followEdges $ start nfa
  where
    followEdges states = S.difference newStates states
      where
        newStates = S.unions
                  $ S.map snd
                  $ S.filter ((`S.member` states) . fst . fst)
                  $ delta nfa

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
extendStatesByRule :: (Ord a) => NFA a b -> (States a -> States a) -> States a -> States a
extendStatesByRule nfa rule = S.unions . takeWhile (not . S.null) . applyRepeatedly rule
  where
    applyRepeatedly f x = x : applyRepeatedly f (f x)

getEmptyReachableStates :: (Ord a, ELabel b) => NFA a b -> States a -> States a
getEmptyReachableStates nfa = extendStatesByRule nfa followEmptyEdges
  where
    followEmptyEdges states = S.difference newStates states
      where
        newStates = S.unions
                  $ S.map snd
                  $ S.filter (isEmptyLabel . snd . fst)
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
renameStates :: (Integral a, Ord b, Ord c) => a -> NFA b c -> NFA a c
renameStates smallest nfa = NFA newStates (sigma nfa) newDelta newStart newFinish
  where
    newStates = S.fromList $ zipWith const [smallest..] $ S.toAscList $ states nfa
    newDelta  = S.map transitionRule2int $ delta nfa
    newStart  = S.map state2int $ start nfa
    newFinish = S.map state2int $ finish nfa
    state2int state = (+smallest) $ fromInteger $ toInteger $ S.findIndex state $ states nfa
    transitionRule2int ((stateArg,symbol),stateSetVal) = ((state2int stateArg,symbol),S.map state2int stateSetVal)



insertNFAasEdge :: (Ord a, Ord b, ELabel b) => NFA a b -> a -> a -> NFA a b -> NFA Int b
insertNFAasEdge nfa fromState toState nfb = undefined

-- replaceEmptyEdges
--
-- converts an NFA into an NFA that recognizes the same language, but does not
-- contain any edges labeled with the empty word.
--
replaceEmptyEdges :: (Ord a, Ord b, ELabel b) => NFA a b -> NFA a b
replaceEmptyEdges nfa = nfa {delta = newDelta, start = newStart}
  where
    newStart   = getEmptyReachableStates nfa (start nfa)
    newDelta   = go (delta nfa) (emptyEdges $ delta nfa)
    emptyEdges = S.filter (isEmptyLabel . snd . fst)
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
replaceWordEdges :: (Integral a, ELabel b) => NFA a b -> NFA a b
replaceWordEdges nfa = go nfa deltaList
  where
    go nfb []     = nfb
    go nfb (e:es) = go (replaceWordEdge nfb e) es
    deltaList     = S.toList $ delta nfa

-- replaceSetValuedEdges
--
-- Makes an equivalent NFA, that is described only by edges pointing to singleton sets,
-- not to sets of size > 1.
--
replaceSetValuedEdges :: (Ord a, Ord b) => NFA a b -> NFA a b
replaceSetValuedEdges nfa = nfa {delta = newDelta}
  where
    wordDelta  = S.filter ((>1) . S.size . snd) $ delta nfa
    addedDelta = S.unions $ S.map split1 wordDelta
    split1 ((q,w),qs) = S.map (\r -> ((q,w),S.singleton r)) qs
    newDelta          = S.union addedDelta $ S.difference (delta nfa) wordDelta

removeStates :: (Ord a, Ord b) => NFA a b -> States a -> NFA a b
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

