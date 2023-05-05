<!--
Copyright: (c) 2023, Alrik Neumann
GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)
-->

# Finite Automations and Regular Languages in Haskell

I am implementing this small module to help me study regular languages and the theory of computation.
The goal is to implement different types of Representations of regular languages and make it possible to translate between them.
So far I have implemented some functionality for [nondeterministic finite automations](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) (NFAs), enclosed in the `NFA.hs` file. [Deterministic finite automations](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) (DFAs) are treated as a special case of NFAs, as they mathematically are.

## NFA.hs

Depends on `Data.Set`

### Creating and analysing NFAs

#### NFA
```haskell
data NFA a
```
The typev-ariable should be an Ord-Type, as pretty much all the functions depend on that.

I chose to implement one of the more general definitions of NFAs which allows for multiple start states, aswell as arrows that, instead of just symbols, take whole words as labels including the empty word. 


#### makeNFA
```haskell
makeNFA :: (Ord a) => [a] -> [SymbolRL] -> [((a,WordRL),[a])] -> [a] -> [a] -> Maybe (NFA a)
```
Convenient way to create an NFA as it takes lists as arguments instead of sets.

In the type signature I write [SymbolRL] instead of WordRL, because the argument is
not to be understood as a word, but as a list of symbols, that will be translated into a
an alphabet i.e. a set of symbols.

#### validNFA
*Not yet implemented!*
```haskell
validNFA :: (Ord a) => NFA a -> Bool
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

### Manipulating NFAs

#### toIntNFA
```haskell
toIntNFA :: (Ord a) => Int -> NFA a -> NFA Int
```
Takes and NFA and converts it into an equivalent NFA, which has Int type states starting
with smallest
This can be useful to "simplify" an NFA by "relabeling" it's states with integers, which otherwise
often could be of Ord-type values.
It also can be used to homogenise the type of different NFAs, which might be necessary for a number
of operations.

#### union
```haskell
union :: (Ord a, Ord b) => NFA a -> NFA b -> NFA Int
```
Takes two NFAs and returns an NFA that recognizes the language which is the union of
the languages recognized by the input NFAs.
While it could have been possible to make the return value of type NFA (a,b), instead of
starting by converting both NFAs to NFA Int.
This would have led to much more complicated code, as I could not just have used the union operation
to merge the two NFAs.

#### replaceEmptyEdges
```haskell
replaceEmptyEdges :: (Ord a) => NFA a -> NFA a
```
converts an NFA into an NFA that recognizes the same language, but does not
contain any edges labeled with the empty word.

#### replaceWordEdges
```haskell
replaceWordEdges :: (Ord a) => NFA a -> NFA Int
```
Replaces all the edges of an NFA that are labeled with words of length > 1, 
in a way, that produces an equivalent NFA, that does not have such edges.

Before the rest of the calculation, we convert the NFA into an NFA
with Int states, as this makes it easier to handle the creation of new states,
which is necessary for this function.

#### replaceSetValueEdges
```haskell
replaceSetValueEdges :: (Ord a) => NFA a -> NFA a
```
Makes an equivalent NFA, that is described only by edges pointing to singleton sets,
not to sets of size > 1.

#### pruneUnreachable
```haskell
pruneUnreachable :: (Ord a) => NFA a -> NFA a
```
Prunes away all the states and edges from an NFA, that can never be reached

### Analysing Recognized Languages

#### accepts
```haskell
accepts :: (Ord a) => NFA a -> WordRL -> Maybe Bool
```
Verify if a word is accepted by a given NFA

Returns Nothing if the word does contain symbols which are not in the alphabet of the NFA.
Otherwise returns Just True or Just False if the word was accepted or not accepted respectively

### Kleene-operations

#### kleeneNumber
```haskell
kleeneNumber :: (Ord a) => Int -> S.Set a -> [[a]]
```
Returns a List of all possible lists with elements from the input set of length of the input number.

#### kleeneStar
```haskell
kleeneStar :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set.

#### kleenePlus
```haskell
kleenePlus :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set, leaves out the empty list.

### Testing and Comparing NFAs

#### acceptSameWord
```haskell
acceptSameWord :: (Ord a) => NFA a -> NFA a -> WordRL -> Bool
```
Checks if two NFAs are equal relative to their acceptance of a word.

#### recognizeSameLanguage
```haskell
recognizeSameLanguage :: (Ord a) => Int -> NFA a -> NFA a -> LanguageRL -> Bool
```
Checks if two NFA are equal relative to their acceptance of the first `n` word of a Language, where `n` is the input number.

#### recognizeLanguageVector
```haskell
recognizeLanguageVector :: (Ord a) => Int -> NFA a -> LanguageRL -> [Maybe Bool]
```
Shows the acceptance of a NFA for the first `n` words of a Language, where `n` is the input number.
