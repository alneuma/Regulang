-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module TestRL () where

import qualified Data.Set as S
import TypesRL
import NFA
import SetExtensionRL
import RegLang
import RegEx
import ShowRL

-----------------------
-- Kleene-operations --
-----------------------

-- returns a List of all possible lists with elements from the input set of length of the input number

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

testTrimNFA = makeNFA [0..10] "0" (zip (map (,['0']) [1,2,3,5,7,8,9,10]) [[2,5],[3],[4],[6],[8],[4],[10],[9]]) [1] [4]

test_001 = toWordNFA (Concat (RE '0') (Concat (RE '0') (RE '1')))
test_0 = toWordNFA (RE '0')
test_1 = toWordNFA (RE '1')
conc01 = test_0 `concatenate` test_1
test_emptySet = toWordNFA EmptySet
test_emptyWord = toWordNFA EmptyWord
