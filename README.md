<!--
Copyright: (c) 2023, Alrik Neumann
GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)
-->

# Regulang

I am implementing this small module to help me study regular languages and the theory of computation.
The goal is to implement different types of representations of regular languages and make it possible to translate between them.
So far I have implemented some functionality for [nondeterministic finite automations](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) (NFAs), enclosed in the `NFA.hs` file. [Deterministic finite automations](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) (DFAs) are treated as a special case of NFAs, as they mathematically are.

### Creating and analysing NFAs

#### NFA
```haskell
data NFA a b
```
Should be `(Ord a, Ord b, ELabel b)`, as most functions depend on this.

NFAs are allowed to have multiple start states.
Different types for `b` will lead to different kinds of NFAs:

`b` is of type `SymbolRL`:
NFAs that accept only single symbols as labels for their edges.

`b` is of type `WordRL`:
NFAs that accept whole words as labels for their edges, including the empty word.

`b` is of type `RegEx`:
NFAs that accept regular expressions as labels for their edges.

So far `SymbolRL`, `WordRL` and `RegEx` are the only instances of `ELabel`

#### makeNFA
```haskell
makeNFA :: (Ord a, Ord b, ELabel b) => [a] -> [SymbolRL] -> [((a,b),[a])] -> [a] -> [a] -> NFA a b
```

Make an NFA without directly applying the data constructor, which would need to be supplied with `Set` type values from `Data.Set`.

In the type signature I write [SymbolRL] instead of WordRL, because the argument is
not to be understood as a word, but as a list of symbols, that will be translated into a
an alphabet i.e. a set of symbols.

#### validNFA
*Not yet implemented!*
```haskell
validNFA :: (Ord a, ELabel b) => NFA a b -> Bool
```
Checks if an NFA is defined correctly.
Returns False if any of the following holds
- start is no subset of states
- finish is no subset of states
- not all left entries of delta have their left entries in states or their right entry as an empty
  list or with a single argument, that is in sigma
- the set of the right entries of delta is not a subset of states
Otherwise returns True.

#### validDFA
*Not yet implemented!*
```haskell
validDFA :: (Ord a) => NFA a -> Bool
```
Checks if an NFA is a valid DFA.

#### getReachableStates
```haskell
getReachableStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, which can be directly or indirectly reached from at least one of the start states.

#### getCoReachableStates
```haskell
getCoReachableStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, from where an acceptance state can be reached directly or indirectly.

#### getTrimStates
```haskell
getTrimStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, that satisfy the condition from `getReachableStates` as well as the one from `getCoReachableStates`.

#### getEmptyReachableStates
```haskell
getEmptyReachableStates :: (Ord a, ELabel b) => NFA a b -> States a -> States a
```
Returns all the states which in a given NFA, from a given set of states are reachable by only traversing empty-labeled edges.

#### toIntNFA
```haskell
toIntNFA :: (Ord a, Ord b) => Int -> NFA a b -> NFA Int b
```
Takes and NFA and converts it into an equivalent NFA, which has `Int` type states starting with the value of the first argument.
This can be useful to "simplify" an NFA by "relabeling" it's states with integers.
It also can be used to homogenise the type of different NFAs, to prepare for other operations.

#### unionNFA
```haskell
unionNFA :: (Ord a, Ord b, Ord c) => NFA a c -> NFA b c -> NFA Int c
```
Takes two NFAs and returns an NFA that recognizes the language which is the union of the languages recognized by the input NFAs.

#### replaceEmptyEdges
```haskell
replaceEmptyEdges :: (Ord a, Ord b, ELabel b) => NFA a b -> NFA a b
```
converts an NFA into an NFA that recognizes the same language, but does not contain any edges labeled with the empty word.

#### replaceWordEdges
```haskell
replaceWordEdges :: (Ord a) => NFA a WordRL -> NFA Int WordRL
```
Replaces all the edges of an NFA that are labeled with words of length > 1, in a way, that produces an equivalent NFA, that does not have such edges.

#### replaceSetValuedEdges
```haskell
replaceSetValuedEdges :: (Ord a, Ord b) => NFA a b -> NFA a b
```
Makes an equivalent NFA, that is described only by edges pointing to singleton sets, not to sets of size > 1.

#### removeStates
```haskell
removeStates :: (Ord a, Ord b) => NFA a b -> States a -> NFA a b
```
Removes all given states from an NFA, as well as the associated Edges.

#### simplify
```haskell
simplify :: (Ord a, Ord b) => NFA a b -> NFA a b
```
Removes all states from an NFA that can never be reached or from where no acceptance state can be reached, as well as the associated edges. The resulting NFA is equivalent to the input NFA.

### Analysing Recognized Languages

#### acceptsNFA
```haskell
acceptsNFA :: (Ord a, ELabel b) => NFA a b -> WordRL -> Maybe Bool
```
Verify if a word is accepted by a given NFA

Returns `Nothing` if the word does contain symbols which are not in the alphabet of the NFA.
Otherwise returns `Just True` or `Just False` if the word was accepted or not accepted respectively

### Kleene-operations

#### kleeneNumber
```haskell
kleeneNumber :: (Ord a) => Int -> S.Set a -> [[a]]
```
Returns a list of all possible lists with elements from the input set of length of the input number.

#### kleeneStar
```haskell
kleeneStar :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set.
The output will always be an infinite list.

#### kleenePlus
```haskell
kleenePlus :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set, leaves out the empty list.
The output will always be an infinite list.

### Testing and Comparing NFAs

#### acceptSameWord
```haskell
acceptSameWord :: (Ord a) => NFA a WordRL -> NFA a WordRL -> WordRL -> Bool
```
Checks if two NFAs are equal relative to their acceptance of a word.

#### recognizeSameLanguage
```haskell
recognizeSameLanguage :: (Ord a) => Int -> NFA a WordRL -> NFA a WordRL -> LanguageRL -> Bool
```
Checks if two NFA are equal relative to their acceptance of the first `n` word of a Language, where `n` is the input number.

#### recognizeLanguageVector
```haskell
recognizeLanguageVector :: (Ord a) => Int -> NFA a WordRL -> LanguageRL -> [Maybe Bool]
```
Shows the acceptance of a NFA for the first `n` words of a Language, where `n` is the input number.
