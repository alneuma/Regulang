<!--
Copyright: (c) 2023, Alrik Neumann
GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)
-->

# Deterministic Finite Automations in Haskell

As I am currently learning about the theory of computation I am implementng this small haskell module, that simulates DFA's. As I progress through the curriculum I plan to extend this module to represent a larger part of the theory.

## Basic functionality

The module revolves around the DFA data constructor which is of kind `(* -> *)`
It should always be fed an Ord-type as argument as this is what pretty much all the functions in this module expect. When making new DFAs the recommended way, checking for this will be done automatically.

All the parts of a DFA which are sets in the mathematical definition of a DFA are implemented as Sets from the Data.Set module. Nonetheless there is a function provided (`makeDFA`) which constructs a new DFA from lists instead of sets, cheking the new DFA's validity while doing so. This should generally be the most convient way to make new DFAs. Checking a DFA's validity after creation is necessary, as all the functions (except `valdiDFA`) in a the module that expect DFAs as arguments do assume their validity and will not recover from getting fed invalid ones.

Symbols (`Symbol`) are implemented as `Char` and words (`WordDFA`) as `String`.

The function `accpets :: Ord a => DFA a -> wordDFA -> Maybe Bool`, checks weather a DFA accepts a word.

## Additional functionality

The functions complementDFA, intersectionDFA and unionDFA can be used to create new dfas which accept complements, intersections or unions of the languages which are accepted by input DFAs.

## Planned functionality

### More DFA and regular languages
- a function, that creates a regular language (as a set) from a DFA

### Regular Expressions
- functions that create regular languages (as sets) from regular expressions
- functions that translate regular expressions into DFAs and vice versa

### functionality for NFAs
- datatype for NFAs
- translate NFAs into DFAs
