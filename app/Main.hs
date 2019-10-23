module Main where

import Lib

import System.Directory
import System.FilePath
import System.Process

appendPath :: String -> FilePath -> IO FilePath
appendPath rest_path fp = return $ fp </> rest_path

removeTempDir :: IO ()
removeTempDir = getHomeDirectory >>= (appendPath "temp_dir") >>= removePathForcibly

createDir :: FilePath -> IO ()
createDir fp = getHomeDirectory >>= (appendPath fp) >>= (createDirectoryIfMissing True)

main :: IO ()
main = do
	-- Change path needed
    let path_to_fp_hw = "Desktop/University/fp-homework"
    -- Change hw needed
    let hw = "hw1"
    -- create temp dir
    removeTempDir
    createDir "temp_dir"
    -- copy solution to a temp_dir
    old_path <- getHomeDirectory >>= (appendPath path_to_fp_hw) 
    new_path <- getHomeDirectory >>= (appendPath "temp_dir")
    (_, _, _, r) <- createProcess (shell $ "cp -r " ++ old_path ++ " " ++ new_path)
    waitForProcess r
    -- create test dir if needed
    test_path <- getHomeDirectory >>= (appendPath "temp_dir/fp-homework") >>= (appendPath hw) >>= (appendPath "test")
    createDir test_path
    -- copy teacher tests
    spec_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "TSpec.hs")
    (_, _, _, r) <- createProcess (shell $ "cp -r " ++ spec_path ++ " " ++ test_path)
    waitForProcess r
    rest_spec_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "TTests")
    (_, _, _, r) <- createProcess (shell $ "cp -r " ++ rest_spec_path ++ " " ++ test_path)
    waitForProcess r
    -- extend cabal file
    project_path <- getHomeDirectory >>= (appendPath "temp_dir/fp-homework") >>= (appendPath hw)
    project_cabal_path <- appendPath (hw ++ ".cabal") project_path
    project_temp_cabal_path <- appendPath "temp.cabal" project_path
    cabal_extend_path <- getCurrentDirectory >>= (appendPath hw) >>= (appendPath "cabal_extend.txt")
    (_, _, _, r) <- createProcess(shell $ "cat " ++ project_cabal_path ++ " " ++ cabal_extend_path ++ " > " ++ project_temp_cabal_path)
    waitForProcess r
    (_, _, _, r) <- createProcess(shell $ "cat " ++ project_temp_cabal_path ++ " > " ++ project_cabal_path)
    waitForProcess r
    (_, _, _, r) <- createProcess(shell $ "rm " ++ project_temp_cabal_path)
    waitForProcess r
    let run_tests = "cd " ++ project_path ++ " && stack test " ++ hw
    (_, _, _, r) <- createProcess(shell run_tests)
    waitForProcess r
    -- return ()
    -- copyFile old_path new_path
