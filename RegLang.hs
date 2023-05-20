-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

{-# LANGUAGE TupleSections #-}

module RegLang (
                -- general purpose
                printRL,
                -- class RegLang functions
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
                simplify,
                -- data constructors
                NFA,
                RegEx,
                -- NFA specific
                makeNFA,
                getReachableStates,
                getCoReachableStates,
                getTrimStates,
                removeStates,
                renameStates,
                replaceEmptyEdges,
                replaceWordEdges,
                replaceSetValuedEdges,
                -- kleene-operations on sets (alphabets)
                kleeneNumber,
                kleeneStar,
                kleenePlus
               ) where

import qualified Data.Maybe     as M  (isJust,fromJust,isNothing) 
import qualified Data.Bifunctor as BF (first,second)
import qualified Data.Set       as S

-- Monad instance for S.Set --

---------------------------------------------
---------------------------------------------
---- TYPE SYNONYMS AND DATA CONSTRUCTORS ----
---------------------------------------------
---------------------------------------------

type SymbolRL           = Char
type WordRL             = [SymbolRL]
emptyWord               = [] :: WordRL
type AlphabetRL         = S.Set SymbolRL
type LanguageRL         = S.Set WordRL
type TransitionRule a b = ((a,b),S.Set a)
type Delta a b          = S.Set (TransitionRule a b)
type States a           = S.Set a

-- NFAs
-- (nondeterministic finite automations)
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

-------------------------------------------
-------------------------------------------
---- GENERAL PURPOSE INTERNAL FUNCTION ----
-------------------------------------------
-------------------------------------------

--------------------
-- TransitionRule --
--------------------

getFrom :: TransitionRule a b -> a
getFrom ((q,_),_) = q

getTo :: TransitionRule a b -> S.Set a
getTo (_,q) = q

getLabel :: TransitionRule a b -> b
getLabel ((_,a),_) = a

---------------------
---------------------
---- TYPECLASSES ----
---------------------
---------------------

------------
-- Elabel --
------------

-- Labels for Edges of NFAs
-- SymbolRL
-- WordRL
-- RegEx

class (Ord a) => ELabel a where
  toRegex            :: a -> RegEx
  -- returns a regex, that describes exactly the provided ELabel
  containedPart      :: a -> WordRL -> Maybe (WordRL,WordRL)
  -- Splits a word into the part at the beginning which is contained in a label
  -- and the rest.
  -- Returns Nothing if the label does not accept the word.
  -- Attention:
  -- Always returns Nothing when the word is empty, even when the label is the empty label.
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
  getAlphabet        :: a -> AlphabetRL
  -- Gets the alphabet used by a label
  word2NFA           :: (Integral b) => WordRL -> NFA b a
  -- makes and NFA that accepts exactly the word provided
  insertEmptyEdge    :: (Ord b) => NFA b a -> b -> b -> NFA b a
  -- Inserts and empty Edge into the nfa pointing from the first provided state
  -- to the second one.
  -- Creates the states if they are not yet in the nfa.
  -- This is trivial for WordRL and RegEx but complicated for SymbolRL

instance ELabel SymbolRL where
  toRegex :: SymbolRL -> RegEx
  toRegex = RE

  containedPart :: SymbolRL -> WordRL -> Maybe (WordRL,WordRL)
  containedPart _ [] = Nothing
  containedPart label (x:xs)
    | label == x = Just ([x],xs)
    | otherwise  = Nothing

  leftAfterTraversal :: SymbolRL -> WordRL -> Maybe (S.Set WordRL)
  leftAfterTraversal _ [] = Nothing
  leftAfterTraversal label (x:xs)
    | label == x = Just $ S.singleton xs
    | otherwise  = Nothing

  isSingletonLabel :: SymbolRL -> Bool
  isSingletonLabel _ = True

  isEmptyLabel :: SymbolRL -> Bool
  isEmptyLabel _ = False

  getAlphabet :: SymbolRL -> AlphabetRL
  getAlphabet    = S.singleton

  word2NFA :: (Integral b) => WordRL -> NFA b SymbolRL
  word2NFA word = makeNFA [0..lengthIntegral] (S.toList $ getAlphabet word) delta [0] [lengthIntegral]
    where
      lengthIntegral = fromInteger $ toInteger $ length word
      delta = zip (zip [0..lengthIntegral] word) (map (:[]) [1..lengthIntegral])

  insertEmptyEdge :: (Ord b) => NFA b SymbolRL -> b -> b -> NFA b SymbolRL
  insertEmptyEdge nfa q q' = nfa {delta = newDelta}
    where
      newDelta = S.union (delta nfa)
               $ S.map (\((_,s),rs) -> ((q,s),rs))
               $ S.filter ((==q') . getFrom)
               $ delta nfa

instance ELabel WordRL where
  toRegex :: WordRL -> RegEx
  toRegex []   = EmptyWord
  toRegex word = foldr1 Concat $ map RE word

  containedPart :: WordRL -> WordRL -> Maybe (WordRL,WordRL)
  containedPart label word
    | lengthMatching == lengthLabel = Just (label,drop lengthLabel word)
    | otherwise                     = Nothing
    where
      lengthLabel    = length label
      lengthMatching = length
                     $ takeWhile (== True)
                     $ zipWith (==) label word

  leftAfterTraversal :: WordRL -> WordRL -> Maybe (S.Set WordRL)
  leftAfterTraversal label word
    | M.isNothing parts = Nothing
    | otherwise         = Just $ S.singleton $ snd $ M.fromJust parts
    where
      parts = containedPart label word

  isSingletonLabel :: WordRL -> Bool
  isSingletonLabel word = length word == 1

  isEmptyLabel :: WordRL -> Bool
  isEmptyLabel = (==) emptyWord

  getAlphabet :: WordRL -> AlphabetRL
  getAlphabet = S.fromList

  word2NFA :: (Integral b) => WordRL -> NFA b WordRL
  word2NFA word = makeNFA [0,1] (S.toList $ getAlphabet word) [((0,word),[1])] [0] [1]

  insertEmptyEdge :: (Ord b) => NFA b WordRL -> b -> b -> NFA b WordRL
  insertEmptyEdge nfa q q' = nfa {states = newStates,
                                  delta  = newDelta}
    where
      newStates = S.insert q $ S.insert q' $ states nfa
      newDelta  = S.insert ((q,""),S.singleton q') $ delta nfa

instance ELabel RegEx where
  toRegex :: RegEx -> RegEx
  toRegex = id
  -- containedPart :: RegEx -> WordRL -> Bool
  -- containedPart = undefined
  -- leftAfterTraversal :: RegEx -> WordRL -> Maybe (S.Set WordRL)
  -- leftAfterTraversal = undefined

  isSingletonLabel :: RegEx -> Bool
  isSingletonLabel (RE _) = True
  isSingletonLabel _      = False

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
  word2NFA word = makeNFA [0,1] (S.toList $ getAlphabet word) [((0,toRegex word),[1])] [0] [1]

  insertEmptyEdge :: (Ord b) => NFA b RegEx -> b -> b -> NFA b RegEx
  insertEmptyEdge nfa q q' = nfa {states = newStates,
                                  delta  = newDelta}
    where
      newStates = S.insert q $ S.insert q' $ states nfa
      newDelta  = S.insert ((q,EmptyWord),S.singleton q') $ delta nfa

------------
-- ShowRL --
------------

-- displaying different types in nice readable output

class ShowRL a where
  showRL :: a -> String
  -- Returns a string that can be used for a nicely formatted view of something.
  printRL :: a -> IO ()
  -- This is like showRL, but also prints the string.
  printRL = putStrLn . showRL

instance ShowRL SymbolRL where
  showRL = show

instance ShowRL WordRL where
  showRL = show

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

instance (Show a) => ShowRL (S.Set a) where
  showRL set
    | S.null set = "{}"
    | otherwise  = "{" ++ show (head elements) ++ concatMap (("," ++) . show) (tail elements) ++ "}"
       where elements = S.toAscList set

-- instance (Show a, ShowRL b) => ShowRL (Delta a b) where
--   showRL = concatMap showRL . S.toList

instance (Show a, ShowRL b) => ShowRL (TransitionRule a b) where
  showRL ((q,s),qs) = "(" ++ show q ++ ", " ++ showRL s ++ ")\t-> " ++ showRL qs ++ "\n"

instance (Show a, ShowRL b) => ShowRL (NFA a b) where
    showRL nfa = "States:\t" ++ showRL    (states nfa) ++ "\n" ++
                 "Sigma:\t"  ++ showRL    (sigma  nfa) ++ "\n" ++
                 "Delta:\t"  ++ deltaString            ++
                 "Start:\t"  ++ showRL    (start  nfa) ++ "\n" ++
                 "Finish:\t" ++ showRL    (finish nfa)
      where
        deltaString = showRL (head elementsDelta) ++ concatMap (("\t" ++) . showRL) (tail elementsDelta)
        elementsDelta = S.toAscList $ delta nfa

-------------
-- RegLang --
-------------

-- class of types that can represent regular languages

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
  -- tries to change the language representation into an
  -- equivalent but simpler one.
  -- What exactly is done depends on the type of representation.

-- Only implemented for NFA a WordRL yet, because the
-- accepts function only works with this type yet.
-- Will be fixed in the future.
instance (Integral a, ELabel b) => RegLang (NFA a b) where
  toList :: (Integral a, ELabel b) => NFA a b -> [WordRL]
  toList nfa = filter ((== Just True) . accepts nfa) (kleeneStar $ sigma nfa)

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
                              $ S.filter (M.isJust . flip containedPart w . snd . fst)
                              $ S.filter (not . isEmptyLabel . snd . fst)
                              $ S.filter ((==q) . fst . fst)
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
                                (M.fromJust $ leftAfterTraversal (snd $ fst edge) w)
                                (snd edge)
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

instance RegLang RegEx where
  addWord :: RegEx -> WordRL -> RegEx
  addWord regex = Union regex . toRegex

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
  simplify (Union  EmptySet r)          = simplify r
  simplify (Union  r EmptySet)          = simplify r
  simplify (Union  r r')                = Union (simplify r) (simplify r')
  simplify (Concat EmptyWord EmptyWord) = EmptyWord
  simplify (Concat EmptySet r)          = EmptySet
  simplify (Concat r EmptySet)          = EmptySet
  simplify (Concat r EmptyWord)         = simplify r
  simplify (Concat EmptyWord r)         = simplify r
  simplify (Concat r r')                = Concat (simplify r) (simplify r')
  simplify (Kleene EmptySet)            = EmptyWord
  simplify (Kleene EmptyWord)           = EmptyWord
  simplify r                            = r

  fromNFA :: (Integral a, ELabel b) => NFA a b -> RegEx
  fromNFA nfa = regex
    where
      regex = undefined
      reNFA = (simplify nfa) {delta = reDelta}
      reDelta = S.map (\((q,l),rs) -> ((q, toRegex l), rs)) $ delta $ simplify nfa
      -- replaceEmptyEdges

-- functionality for converting NFAs to RegEx

toRegEx :: (Integral a, ELabel b) => NFA a b -> RegEx
toRegEx nfa = getLabel
            $ head
            $ S.toList newDelta
  where
    tmpNFA   = toGNFA nfa
    tmpDelta = toSingleDestinatonDelta $ delta tmpNFA
    statesToRemove = drop 2
                   $ S.toList
                   $ states tmpNFA
    newDelta = go tmpDelta statesToRemove
      where
        go d []     = d
        go d (q:qs) = go (removeStateFromDelta d q) qs

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
    newNFA    = from2NFA {delta = S.map (\((q,a),r) -> ((q,toRegex a),r)) $ delta from2NFA}
    from2NFA  = renameStates 2 nfa
    newStates = S.union (S.fromList [0,1]) (states newNFA)
    newStart  = S.singleton 0
    newFinish = S.singleton 1
    newDelta  = S.unions [delta newNFA, fromNewStartEmptyWord, fromNewStartEmptySet, toNewFinishEmptyWord, toNewFinishEmptySet]
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
             $ from `S.cartesianProduct` to
    regexMid = foldr Concat EmptyWord
             $ S.map getLabel fromTo
    (from,fromTo,to,other) = splitDelta S.empty S.empty S.empty S.empty
                           $ S.toList delta
      where
        splitDelta from fromTo to other [] = (from,fromTo,to,other)
        splitDelta from fromTo to other (e:es)
          | p && q    = splitDelta from (S.insert e fromTo) to other es
          | p         = splitDelta (S.insert e from) fromTo to other es
          | q         = splitDelta from fromTo (S.insert e to) other es
          | otherwise = splitDelta from fromTo to (S.insert e other) es
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

instance RegLang Grammar where
  -- yet to be implemented

-------------
-------------
---- NFA ----
-------------
-------------

------------------------------------------------
-- functions for creating and validating NFAs --
------------------------------------------------

-- Convenient way to create an NFA as it takes lists as arguments instead of sets.
--
-- In the type signature I write [SymbolRL] instead of WordRL, because the argument is
-- not to be understood as a word, but as a list of symbols, that will be translated into a
-- an alphabet i.e. a set of symbols.
makeNFA :: (Ord a, Ord b, ELabel b) => [a] -> [SymbolRL] -> [((a,b),[a])] -> [a] -> [a] -> NFA a b
makeNFA states sigma delta start finish = NFA newStates newSigma newDelta newStart newFinish
  where
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
--
-- Before the the rest of the calculation, we convert the NFA into an NFA
-- with Int states, as this makes it easier to handle the creation of new states,
-- which is necessary for this function.
--
replaceWordEdges :: (Ord a) => NFA a WordRL -> NFA Int WordRL
replaceWordEdges nfa = tmpNFA {states = newStates, delta = newDelta}
  where
    tmpNFA               = renameStates 0 nfa
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

acceptSameWord :: (Integral a) => NFA a WordRL -> NFA a WordRL -> WordRL -> Bool
acceptSameWord nfa nfb word = accepts nfa word == accepts nfb word

recognizeSameLanguage :: (Integral a) => Int -> NFA a WordRL -> NFA a WordRL -> LanguageRL -> Bool
recognizeSameLanguage n nfa nfb = all (acceptSameWord nfa nfb) . take n . S.toList

recognizeLanguageVector :: (Integral a) => Int -> NFA a WordRL -> LanguageRL -> [Maybe Bool]
recognizeLanguageVector n nfa = take n . map (accepts nfa) . S.toList

------------------------------
-- example and testing NFAs --
------------------------------

-- zeros2div3div
-- an NFA that has an alphabet that consists only of '0' and accepts any number of '0's that is divisible by 2 or 3
zeros2div3div = makeNFA [0,1,2,3,4,5] ['0'] [((0,emptyWord),[1,3]),((1,['0']),[2]),((2,['0']),[1]),((3,['0']),[4]),((4,['0']),[5]),((5,['0']),[3])] [0] [1,3]

zeros2div3divInt = renameStates 0 zeros2div3div

-- notZeros2div3div = complementNFA zeros2div3div

even1s = makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[1]),((0,['1']),[1]),((1,['1']),[0])] [0] [0]

endsWith1 = makeNFA [0,1] ['0','1'] [((0,['0']),[0]),((1,['0']),[0]),((0,['1']),[1]),((1,['1']),[1])] [0] [1]

abc = makeNFA [0,1] ['a','b','c'] [((0,"abc"),[1])] [0] [1]

abcNoWords = replaceWordEdges abc

unionEndsWith1even1s = endsWith1 `union` even1s

-- notEndsWith1 = complementNFA endsWith1

-- notEven1s = complementNFA even1s

-- intersectionEndsWith1even1s = intersectionNFA endsWith1 even1s

zeros2div3divNoEps = replaceEmptyEdges zeros2div3div

-- testReplaceEmptyEdges = recognizeSameLanguage 20 zeros2div3div zeros2div3divNoEps $ S.toList $ kleeneStar $ S.singleton '0'

testTrimNFA = makeNFA [0..10] "0" (zip (zip [1,2,3,5,7,8,9,10] $ repeat ['0']) [[2,5],[3],[4],[6],[8],[4],[10],[9]]) [1] [4]

test_001 = toWordNFA (Concat (RE '0') (Concat (RE '0') (RE '1')))
test_0 = toWordNFA (RE '0')
test_1 = toWordNFA (RE '1')
conc01 = test_0 `concatenate` test_1
test_emptySet = toWordNFA EmptySet
test_emptyWord = toWordNFA EmptyWord
