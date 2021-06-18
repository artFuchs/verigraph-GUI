module GUI.Helper.FilePath
( getResourcesFolder )
where

-- haskell system modules
import           System.Directory
import           System.FilePath


getResourcesFolder :: IO FilePath
getResourcesFolder = do
  let globalDir = "/usr/share/verigraph-GUI/Resources/"
  localDir <- getCurrentDirectory >>= \p -> return $ joinPath $ (take 3 $ splitPath p) ++ [".local/share/verigraph-GUI/Resources/"]
  globalDirExists <- doesDirectoryExist globalDir
  localDirExists  <- doesDirectoryExist localDir
  haveResourcesHere <- doesDirectoryExist "./Resources/"
  return $ case (globalDirExists, localDirExists, haveResourcesHere) of
    (_   ,_    ,True ) -> "./Resources/"
    (_   ,True ,False) -> localDir
    (True,False ,False) -> globalDir
    _ -> error "Resources folder not found."
