module Tools.Bib.IO where

import System.Directory
import System.FilePath
import Control.Monad
import Tools.Bib.BibTex as B

yamlExt :: [String]
yamlExt = ["yaml", "yml"]

convertYamlToBib :: FilePath -> FilePath -> IO ()
convertYamlToBib y b = B.decodeRequestFile y >>= writeBibFile b

convertYamlsToBibs :: FilePath -> IO [FilePath]
convertYamlsToBibs root = do
  -- Prepare the directories
  let bibDir = root </> "bibliography"
  let bibOutDir = bibDir </> "out"
  createDirectoryIfMissing True bibOutDir
  -- Loop through the yaml files.
  yamls <- listYamlFiles bibDir
  forM yamls $ \y -> do
    let src = bibDir </> y
    let target = bibOutDir </> (y -<.> "bib")
    convertYamlToBib src target
    return target

anyExtOf :: Foldable t => t String -> FilePath -> Bool
anyExtOf es f = any (`isExtensionOf` f) es

listYamlFiles :: FilePath -> IO [FilePath]
listYamlFiles f = filter (anyExtOf yamlExt) <$> listDirectory f
