<!--
Copyright: (c) 2023, Alrik Neumann
GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)
-->

# Finite Automations and Regular Languages in Haskell

I am implementing these small modules to help me study theory of computation.
There are two modules:

`DFA.hs` implements [deterministic finite automations](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) (DFAs) only. I have written this first and the code is likely less clean.

`NFA.hs` implements the more general case of [non-deterministic finite automations](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) (NFAs) and treats DFAs merely as a special case of NFAs, as they mathematically are.

## DFA.hs

### Basic functionality

The module revolves around the DFA data constructor which is of kind `(* -> *)`
It should always be fed an Ord-type as argument. This type-variable is the type, which is used to represent the states of the DFA.

All the parts of a DFA which are sets in the mathematical definition of a DFA are implemented as Sets from the Data.Set module. Nonetheless there is a function provided (`makeDFA`) which constructs a new DFA from lists instead of sets, checking the new DFA's validity while doing so. This should generally be the most convenient way to make new DFAs. Checking a DFA's validity after creation is necessary, as all the functions (except `valdiDFA`) that expect DFAs as arguments do assume their validity and will generally not recover from getting fed invalid ones.

Symbols (`Symbol`) are implemented as `Char` and words (`WordDFA`) as `String`.

The function `accpets :: Ord a => DFA a -> wordDFA -> Maybe Bool`, checks weather a DFA accepts a word.

### Additional functionality

The functions `complementDFA`, `intersectionDFA` and `unionDFA` can be used to create new DFAs which accept complements, intersections or unions of the languages which are accepted by input DFAs.

## NFA.hs

This implements the NFA data constructor `(* -> *)`, which like the DFA data constructor takes a type-variable of class Ord, which is used to represent the states of the NFA.

I chose to implement one of the more general definitions of NFAs which allows for multiple start states, swell as arrows, which take whole words as labels, including the empty word, instead of just symbols.

`makeNFA` and `acceptsNFA` work like their equivalents in `DFA.hs`.

Currently I am working at functionality that will tranform any NFA into an NFA that only has words of length 1 as labels on it's arrows but recognizes the same language.
