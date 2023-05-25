-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module RegEx () where

import ELabel
import RegLang
import TypesRL
import ShowRL
import NFA
import qualified Data.Set as S

instance RegLang RegEx where
  addWord :: RegEx -> WordRL -> RegEx
  addWord regex = Union regex . label2RegEx

  union :: RegEx -> RegEx -> RegEx
  union = Union

  concatenate :: RegEx -> RegEx -> RegEx
  concatenate = Concat

  kleene :: RegEx -> RegEx
  kleene = Kleene

  toWordNFA :: (Integral a) => RegEx -> NFA a WordRL
  toWordNFA (RE x)       = NFA (S.fromList [0,1])
                               (S.singleton x)
                               (S.singleton ((0,[x]),S.singleton 1))
                               (S.singleton 0)
                               (S.singleton 1)
  toWordNFA EmptySet     = NFA (S.singleton 0)
                                S.empty
                                S.empty
                               (S.singleton 0)
                                S.empty
  toWordNFA EmptyWord    = NFA (S.singleton 0)
                                S.empty
                                S.empty
                               (S.singleton 0)
                               (S.singleton 0)
  toWordNFA (Kleene x)   = kleene $ toWordNFA x
  toWordNFA (Union x y)  = toWordNFA x `union` toWordNFA y
  toWordNFA (Concat x y) = toWordNFA x `concatenate` toWordNFA y

  accepts :: RegEx -> WordRL -> Maybe Bool
  accepts regex = accepts (toWordNFA regex)

  simplify :: RegEx -> RegEx
  simplify x@(RE _)             = x
  simplify EmptySet             = EmptySet
  simplify EmptyWord            = EmptyWord
  simplify (Union r r')         = simplifyRegExLight (Union (simplify r) (simplify r'))
  simplify (Kleene x@(RE _))    = Kleene x
  simplify (Kleene r)           = simplifyRegExLight (Kleene $ simplify r)
  simplify (Concat r EmptyWord) = simplify r
  simplify (Concat EmptyWord r) = simplify r
  simplify (Concat r r')        = simplifyRegExLight (Concat (simplify r) (simplify r'))

  fromNFA :: (Integral a, ELabel b) => NFA a b -> RegEx
  fromNFA nfa = getLabel
              $ head
              $ S.toList
              $ unifyLabelsDelta newDelta
    where
      tmpNFA   = toGNFA $ simplify $ replaceEmptyEdges nfa
      tmpDelta = toSingleDestinatonDelta $ delta tmpNFA
      statesToRemove = drop 2
                     $ S.toList
                     $ states tmpNFA
      newDelta = go tmpDelta statesToRemove
        where
          go d []     = d
          go d (q:qs) = go (removeStateFromDelta d q) qs

-- helper-function for simplify (RegEx)
simplifyRegExLight :: RegEx -> RegEx
simplifyRegExLight (Kleene EmptySet) = EmptyWord
simplifyRegExLight (Kleene EmptyWord) = EmptyWord
simplifyRegExLight (Union EmptySet r) = simplify r
simplifyRegExLight (Union r EmptySet) = simplify r
simplifyRegExLight (Concat EmptySet r) = EmptySet
simplifyRegExLight (Concat r EmptySet) = EmptySet
simplifyRegExLight r = r


instance ELabel RegEx where
  label2RegEx :: RegEx -> RegEx
  label2RegEx = id

  containedPart :: RegEx -> WordRL -> Maybe (WordRL,WordRL)
  containedPart _     emptyWord = Nothing
  containedPart EmptyWord _     = Nothing
  containedPart EmptySet  _     = Nothing
  containedPart regex word = go regex [head word] (tail word)
    where
      go re acc []
        | test == Just True = Just (toTest,[])
        | otherwise         = Nothing
        where
          test   = accepts re toTest
          toTest = reverse acc
      go re acc remainingWord@(w:ws)
        | test == Just True = Just (toTest,remainingWord)
        | otherwise         = go re (w:acc) ws
        where
          test   = accepts re toTest
          toTest = reverse acc

  leftAfterTraversal :: RegEx -> WordRL -> Maybe (S.Set WordRL)
  leftAfterTraversal = undefined

  isSingletonLabel :: RegEx -> Bool
  isSingletonLabel (RE _) = True
  isSingletonLabel _      = False

  singletonToSymbol :: RegEx -> Maybe SymbolRL
  singletonToSymbol (RE x) = Just x
  singletonToSymbol _      = Nothing

  isEmptyLabel :: RegEx -> Bool
  isEmptyLabel (RE _)        = False
  isEmptyLabel EmptySet      = False
  isEmptyLabel EmptyWord     = True
  isEmptyLabel (Kleene r)    = True
  isEmptyLabel (Concat r r') = isEmptyLabel r && isEmptyLabel r'
  isEmptyLabel (Union  r r') = isEmptyLabel r || isEmptyLabel r'

  getAlphabet :: RegEx -> AlphabetRL
  getAlphabet (RE s)        = S.singleton s
  getAlphabet EmptySet      = S.empty
  getAlphabet EmptyWord     = S.empty
  getAlphabet (Kleene r)    = getAlphabet r
  getAlphabet (Concat r r') = S.union (getAlphabet r) (getAlphabet r')
  getAlphabet (Union  r r') = S.union (getAlphabet r) (getAlphabet r')

  word2NFA :: (Integral b) => WordRL -> NFA b RegEx
  word2NFA word = makeNFA [0,1] (S.toList $ getAlphabet word) [((0,label2RegEx word),[1])] [0] [1]

  insertEmptyEdge :: (Ord b) => NFA b RegEx -> b -> b -> NFA b RegEx
  insertEmptyEdge nfa q q' = nfa {states = newStates,
                                  delta  = newDelta}
    where
      newStates = S.insert q $ S.insert q' $ states nfa
      newDelta  = S.insert ((q,EmptyWord),S.singleton q') $ delta nfa

instance ShowRL RegEx where
-- "{}" for empty-set-symbol
-- "''" for empty-word-symbol (epsilon)
-- "|"  for set-union-symbol
-- everything else should be as expected
  showRL (RE x)                                = [x]
  showRL EmptySet                              = "{}"
  showRL EmptyWord                             = "''"
  showRL (Kleene (RE x))                       = [x,'*']
  showRL (Kleene r)                            = "(" ++ showRL r ++ ")*"
  showRL (Concat r@(Union _ _) r'@(Union _ _)) = "(" ++ showRL r ++ ")(" ++ showRL r' ++ ")"
  showRL (Concat r@(Union _ _) r')             = "(" ++ showRL r ++ ")" ++ showRL r'
  showRL (Concat r             r'@(Union _ _)) = showRL r ++ "(" ++ showRL r' ++ ")"
  showRL (Concat r r')                         = showRL r ++ showRL r'
  showRL (Union  r r')                         = showRL r ++ "|" ++ showRL r'


-- toGNFA
--
-- Converts an NFA into an equivalent NFA with RegEx labels.
-- The new NFA will only have a single start- and a single acceptance-state.
-- All states of the new NFA which are neither acceptance nor start state,
-- will be connected with incoming edges from the start state and with
-- outgoing edges from the acceptance state.
toGNFA :: (Integral a, ELabel b) => NFA a b -> NFA a RegEx
toGNFA nfa = newNFA {states = newStates,
                     delta  = newDelta,
                     start  = newStart,
                     finish = newFinish}
  where
    newNFA    = from2NFA {delta = S.map (\((q,a),r) -> ((q,label2RegEx a),r)) $ delta from2NFA}
    from2NFA  = renameStates 2 nfa
    newStates = S.union (S.fromList [0,1]) (states newNFA)
    newStart  = S.singleton 0
    newFinish = S.singleton 1
    newDelta  = toSingleDestinatonDelta
              $ S.unions [delta newNFA,
                          fromNewStartEmptyWord,
                          fromNewStartEmptySet,
                          toNewFinishEmptyWord,
                          toNewFinishEmptySet]
    fromNewStartEmptyWord = S.singleton ((0,EmptyWord),start newNFA)
    fromNewStartEmptySet  = S.singleton ((0,EmptySet), states newNFA `S.difference` start newNFA)
    toNewFinishEmptyWord  = S.map (\q -> ((q,EmptyWord),S.singleton 1))
                          $ finish newNFA
    toNewFinishEmptySet   = S.map (\q -> ((q,EmptySet),S.singleton 1))
                          $ states newNFA `S.difference` finish newNFA

-- second entries of all members of delta need to be singleton sets
removeStateFromDelta :: (Integral a) => Delta a RegEx -> a -> Delta a RegEx
removeStateFromDelta delta state = newDelta
  where
    newDelta = S.union newEdges other
    newEdges = S.map (\(((q,regexQ),_),((_,regexR),r)) -> ((q,Concat regexQ (Concat regexMid regexR)),r))
             $ to `S.cartesianProduct` from
    regexMid = simplify
             $ foldr Concat EmptyWord
             $ S.map getLabel toFrom
    (to,toFrom,from,other) = splitDelta S.empty S.empty S.empty S.empty
                           $ S.toList delta
      where
        splitDelta to toFrom from other [] = (to,toFrom,from,other)
        splitDelta to toFrom from other (e:es)
          | p && q    = splitDelta to (S.insert e toFrom) from other es
          | q         = splitDelta (S.insert e to) toFrom from other es
          | p         = splitDelta to toFrom (S.insert e from) other es
          | otherwise = splitDelta to toFrom from (S.insert e other) es
          where
            p = state == getFrom e
            q = state `S.member` getTo e

-- splits all the edges into singleton-valued ones
toSingleDestinatonDelta :: (Ord a, Ord b) => Delta a b -> Delta a b
toSingleDestinatonDelta = S.unions . S.map (\((q,e),rs) -> S.map (((q,e),) . S.singleton) rs)

-- unifies edges that go from the same states to the same states
-- into equivalent single edges by using the union of the labels.
unifyLabelsDelta :: (Ord a) => Delta a RegEx -> Delta a RegEx
unifyLabelsDelta delta
  | S.null delta = delta
  | otherwise    = go S.empty [] (tail deltaList) (head deltaList)
    where
      deltaList = S.toList delta
      go newDelta []       []     key = S.insert key newDelta
      go newDelta (t:ts)   []     key = go (S.insert key newDelta) [] ts t
      go newDelta rulesAcc (u:us) key
        | sameFromTo u key = go newDelta rulesAcc us (unify u key)
        | otherwise        = go newDelta (u:rulesAcc) us key
        where
          sameFromTo ((q,_),r) ((q',_),r') = q == q' && r == r'
          unify      ((q,e),r) ((_,f),_)   = ((q,Union e f),r)

