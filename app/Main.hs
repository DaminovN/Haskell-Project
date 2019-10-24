module Main where

import Lib

import System.Environment
import System.Directory
import System.FilePath((</>))
import System.Process
import Control.Applicative((<$>))
import Control.Exception(throw)
import Control.Monad(when,forM_)
import System.IO

appendPath :: String -> FilePath -> IO FilePath
appendPath rest_path fp = return $ fp </> rest_path

removeTempDir :: IO ()
removeTempDir = getHomeDirectory >>= (appendPath "temp_dir") >>= removePathForcibly

createDir :: FilePath -> IO ()
createDir fp = getHomeDirectory >>= (appendPath fp) >>= (createDirectoryIfMissing True)



copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $
    throw (userError "destination already exists")

  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r


-- argument 1 path to student solution, example: "/Users/daminovn/Desktop/University/fp-homework"
-- argument 2 hw, example: "hw1"
main :: IO ()
main = do
    app_args <- getArgs
    let path_to_fp_hw = app_args!!0
    let hw = app_args!!1
    -- create temp dir
    removeTempDir
    createDir "temp_dir"
    -- copy solution to a temp_dir
    new_fp_hw_path <- getHomeDirectory >>= (appendPath "temp_dir/fp-homework")
    copyDir path_to_fp_hw new_fp_hw_path

    -- create test dir if needed
    let test_path = new_fp_hw_path </> hw </> "test"
    createDir test_path
    -- copy teacher tests
    spec_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "TSpec.hs")
    copyFile spec_path (test_path </> "TSpec.hs")
    rest_spec_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "TTests")
    copyDir rest_spec_path (test_path </> "TTests")

    -- extend cabal file
    cabal_extend_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "cabal_extend.txt")
    let project_cabal_path = new_fp_hw_path </> hw </> (hw ++ ".cabal")
    extend_content <- readFile cabal_extend_path
    appendFile project_cabal_path extend_content

