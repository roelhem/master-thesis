module Main where

import qualified Tools.Bib.IO as B
import System.Directory (getCurrentDirectory)

main :: IO ()
main = getCurrentDirectory >>= B.convertYamlsToBibs >> return ()
