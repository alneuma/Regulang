<!--
Copyright: (c) 2023, Alrik Neumann
GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)
-->

# Regulang

I am implementing this small module to help me study regular languages and the theory of computation.
The goal is to implement different types of representations of regular languages and make it possible to translate between them.
So far I have implemented some functionality for [nondeterministic finite automations](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) (NFAs), enclosed in the `NFA.hs` file. As well as some functionality for regular expressions.
[Deterministic finite automations](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) (DFAs) are treated as a special case of NFAs, as they mathematically are.

<!--
## Theory

### Deterministic Finite Automations

When a word is a finite sequence of symbols, then a *regular language* is a set of words, that can be fully described by a *deterministic finite automation* (DFA). These automations can be imagined as a set of states (nodes) and a number of transition rules (edges) which, given a state, determine to which next state to progress when reading a specific symbol as part of a word.
Each DFA has exactly one start-state and can have zero or more acceptance-states. When by reading a word symbol by symbol we can traverse the DFA from the start-state to one of the acceptance-states, we say that the DFA accepts the word, or that the word is part of the language described by the DFA.

DFAs are mathematically formalised as five-tuples $(Q,\Sigma,\delta,q_{0},F)$, where $Q$ is the set of states, $\Sigma$ is the alphabet of allowed symbols, $\delta \colon Q \times \Sigma \to Q$ is the transition function (or set of edges), $q_{0} \in Q$ is the start state and $F \subset Q$ are the acceptance-states.

With this formalization also comes a more precise notion, of what it means for an NFA to accept a word:
A word $w = w_{1}..w_{n}$ with $w_{i} \in \Sigma$ for all $i \in \{1,..,n\}$ is accepted by a given DFA when there is a sequence of states $r_{0},..,r_{n}$, with $r_{i} \in Q$ for all $i \in \{0,..,n\}$, such that the following three conditions hold:

1. $r_{0} = q_{0}$
2. $\forall i \in \\{1,..,n\\} \colon r_{i} = \delta(r_{i-1},w_{i})$
3. $r_{n} \in F$

### Nondeterministic Finite Automations

For *Nondeterministic Finite Automations* (NFAs) there are different definitions, varying in their degree of generality i.e. by what kinds of construct they allow. Still they are all equal in the sense, that every less general version can be interpreted as a special case of one of the more general versions and that any NFA that satisfies one of the more general definitions can be translated into an equivalent NFA stasfying one of the narrower definitions. Finally also every DFA, can be seen interpreted as just special kind of NFA and there exists an algorithm by which any NFA can be transformed into an equivalent DFA. That two finite automations are equivalent means here, that they accept the same language.

For this module I chose to implement one of the more general definitions of NFAs as a default. While it's formalization is mostly equal to the one of the DFA. There are a couple of important differences.

1. Instead of a single start state $q_{0}$ there is a set of start states $S$.
2. $\delta$ now is a function that maps from $(Q \times \Sigma\*)$ to $P(Q)$, where $\Sigma\*$ is the set of all words composable from symbols in $\Sigma$ and $P(Q)$ is the powerset of $Q$.
3. The conditions for a word being accepted are the similar to the ones for DFAs, with the difference,
that instead of the first condition $r_{0}$ needs to be a member of $S$ and that the second condition reads 
$\forall i 'in \{1,..,n\} \colon r_{i} \in \delta(r_{i-1},w_{i})$, as appropriate to the new range of $\delta$.

The intuition for NFAs defined in this way is as follows:
Instead of having a *eindeutig* set of instructions, that tells us from which state to change to which other state, when a certain symbol is read, it is possible that reading a symbol allows one to choose between a number of potential follow-up states. In case of $\delta$ mapping to $\emptysey \in P(Q)$ for a given state and symbol, this number can also be $0$. If for a given word, there is at least one choosable path leading from one of the start states to one of the acceptance-states. The word is accepted by the NFA.
Another way to look at it is by imagining following different paths through the NFA at once, such that execution does not proceed from states to states, but from a sets of states to sets of states.
By replacing $\Sigma$ by $\Sigma\*$ in the domain of $\delta$... [to be continued]
-->

## Implementation

### The RegLang Typeclass

```haskell
class RegLang a where
  accepts      :: a -> WordRL -> Maybe Bool
  toList       :: a -> [WordRL]
  toWordNFA    :: (Integral b) => a -> NFA b WordRL
  fromWordNFA  :: (Integral b) => NFA b WordRL -> a
  addWord      :: a -> WordRL -> a
  union        :: a -> a -> a
  intersection :: a -> a -> a
  difference   :: a -> a -> a
  concat       :: a -> a -> a
  kleene       :: a -> a
```

#### Instances

So far there are three instances of `RegLang`:

##### NFAs
```haskell
instance (Integral a, ELabel b) => RegLang (NFA a b)
```
Besides `fromList`, `intersection` and `difference` this is fully implemented.

`ELabel` is a class for types that can be used as labels for the edges of an NFA. Instances are `Symbol RL`, `WordRL` and `RegEx`.

##### regular expressions
```haskell
instance RegLang RegEx
```
Only `addWord`, `kleene`, `concat` and `union` are implemented.

##### grammars
```haskell
instance RegLang Grammar
```
Nothing implemented yet.

### NFAs

#### Creating and analysing NFAs

##### NFA
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

##### makeNFA
```haskell
makeNFA :: (Ord a, Ord b, ELabel b) => [a] -> [SymbolRL] -> [((a,b),[a])] -> [a] -> [a] -> NFA a b
```

Make an NFA without directly applying the data constructor, which would need to be supplied with `Set` type values from `Data.Set`.

In the type signature I write [SymbolRL] instead of WordRL, because the argument is
not to be understood as a word, but as a list of symbols, that will be translated into a
an alphabet i.e. a set of symbols.

##### validNFA
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

##### validDFA
*Not yet implemented!*
```haskell
validDFA :: (Ord a) => NFA a -> Bool
```
Checks if an NFA is a valid DFA.

##### getReachableStates
```haskell
getReachableStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, which can be directly or indirectly reached from at least one of the start states.

##### getCoReachableStates
```haskell
getCoReachableStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, from where an acceptance state can be reached directly or indirectly.

##### getTrimStates
```haskell
getTrimStates :: (Ord a) => NFA a b -> States a
```
Returns all the states of an NFA, that satisfy the condition from `getReachableStates` as well as the one from `getCoReachableStates`.

##### getEmptyReachableStates
```haskell
getEmptyReachableStates :: (Ord a, ELabel b) => NFA a b -> States a -> States a
```
Returns all the states which in a given NFA, from a given set of states are reachable by only traversing empty-labeled edges.

##### toIntNFA
```haskell
toIntNFA :: (Ord a, Ord b) => Int -> NFA a b -> NFA Int b
```
Takes and NFA and converts it into an equivalent NFA, which has `Int` type states starting with the value of the first argument.
This can be useful to "simplify" an NFA by "relabeling" it's states with integers.
It also can be used to homogenise the type of different NFAs, to prepare for other operations.

##### unionNFA
```haskell
unionNFA :: (Ord a, Ord b, Ord c) => NFA a c -> NFA b c -> NFA Int c
```
Takes two NFAs and returns an NFA that recognizes the language which is the union of the languages recognized by the input NFAs.

##### replaceEmptyEdges
```haskell
replaceEmptyEdges :: (Ord a, Ord b, ELabel b) => NFA a b -> NFA a b
```
converts an NFA into an NFA that recognizes the same language, but does not contain any edges labeled with the empty word.

##### replaceWordEdges
```haskell
replaceWordEdges :: (Ord a) => NFA a WordRL -> NFA Int WordRL
```
Replaces all the edges of an NFA that are labeled with words of length > 1, in a way, that produces an equivalent NFA, that does not have such edges.

##### replaceSetValuedEdges
```haskell
replaceSetValuedEdges :: (Ord a, Ord b) => NFA a b -> NFA a b
```
Makes an equivalent NFA, that is described only by edges pointing to singleton sets, not to sets of size > 1.

##### removeStates
```haskell
removeStates :: (Ord a, Ord b) => NFA a b -> States a -> NFA a b
```
Removes all given states from an NFA, as well as the associated Edges.

##### simplify
```haskell
simplify :: (Ord a, Ord b) => NFA a b -> NFA a b
```
Removes all states from an NFA that can never be reached or from where no acceptance state can be reached, as well as the associated edges. The resulting NFA is equivalent to the input NFA.

#### Analysing Recognized Languages

##### acceptsNFA
```haskell
acceptsNFA :: (Ord a, ELabel b) => NFA a b -> WordRL -> Maybe Bool
```
Verify if a word is accepted by a given NFA

Returns `Nothing` if the word does contain symbols which are not in the alphabet of the NFA.
Otherwise returns `Just True` or `Just False` if the word was accepted or not accepted respectively

#### Kleene-operations

##### kleeneNumber
```haskell
kleeneNumber :: (Ord a) => Int -> S.Set a -> [[a]]
```
Returns a list of all possible lists with elements from the input set of length of the input number.

##### kleeneStar
```haskell
kleeneStar :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set.
The output will always be an infinite list.

##### kleenePlus
```haskell
kleenePlus :: (Ord a) => S.Set a -> [[a]]
```
Returns a list of all finite lists composable by the elements of the input set, leaves out the empty list.
The output will always be an infinite list.

#### Testing and Comparing NFAs

##### acceptSameWord
```haskell
acceptSameWord :: (Ord a) => NFA a WordRL -> NFA a WordRL -> WordRL -> Bool
```
Checks if two NFAs are equal relative to their acceptance of a word.

##### recognizeSameLanguage
```haskell
recognizeSameLanguage :: (Ord a) => Int -> NFA a WordRL -> NFA a WordRL -> LanguageRL -> Bool
```
Checks if two NFA are equal relative to their acceptance of the first `n` word of a Language, where `n` is the input number.

##### recognizeLanguageVector
```haskell
recognizeLanguageVector :: (Ord a) => Int -> NFA a WordRL -> LanguageRL -> [Maybe Bool]
```
Shows the acceptance of a NFA for the first `n` words of a Language, where `n` is the input number.
