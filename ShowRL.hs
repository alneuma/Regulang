-- Copyright: (c) 2023, Alrik Neumann
-- GNU General Public License v3.0+ (see LICENSE.txt or https://www.gnu.org/licenses/gpl-3.0.txt)

module ShowRL (ShowRL,showRL,printRL,printDelta,showDelta) where

import qualified Data.Set as S
import TypesRL

class ShowRL a where
  showRL :: a -> String
  -- Returns a string that can be used for a nicely formatted view of something.
  printRL :: a -> IO ()
  -- This is like showRL, but also prints the string.
  printRL = putStrLn . showRL

instance (Show a) => ShowRL (S.Set a) where
  showRL set
    | S.null set = "{}"
    | otherwise  = "{" ++ show (head elements) ++ concatMap (("," ++) . show) (tail elements) ++ "}"
       where elements = S.toAscList set

instance (Show a, ShowRL b) => ShowRL (TransitionRule a b) where
  showRL ((q,s),qs) = "(" ++ show q ++ ", " ++ showRL s ++ ")\t-> " ++ showRL qs ++ "\n"

printDelta :: (Show a, ShowRL b) => Delta a b -> IO ()
printDelta = putStrLn . showDelta

showDelta :: (Show a, ShowRL b) => Delta a b -> String
showDelta = concatMap showRL . S.toList

