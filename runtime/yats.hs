--
-- Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- YATS: Yet Another Test Suite: Runtime
--

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment(withArgs)
import System.Process
import System.Exit
import System.Posix.Files
import System.IO.Unsafe
import System.Directory
import System.Console.ANSI
import System.Console.CmdArgs

import Control.Monad

import Data.List
import Data.Char
import Data.Maybe

type CompOpt     = String
type Source      = String
type TestVerdict = (ExitCode, ExitCode, ExitCode)

verdictOk = (ExitSuccess, ExitSuccess, ExitSuccess)

data Compiler = Gcc | Clang deriving (Show, Eq, Ord)

data Options = Options
               {
                    verbose :: Bool,
                    others  :: [String]

               } deriving (Data, Typeable, Show)

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
          {
            verbose = False  &= help "verbose",
            others = []      &= args
          } &= summary ("yats " ++ yatsVersion ++ ". Usage: yats [OPTIONS] -- files... [compiler OPT]") &= program "yats"

yatsVersion :: String
yatsVersion = "runtime v1.6"

magenta = setSGRCode [SetColor Foreground Vivid Magenta]
blue    = setSGRCode [SetColor Foreground Vivid Blue]
cyan    = setSGRCode [SetColor Foreground Vivid Cyan]
green   = setSGRCode [SetColor Foreground Vivid Green]
red     = setSGRCode [SetColor Foreground Vivid Red]
bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


main :: IO ()
main = do
    opts <- cmdArgsRun options
    when (null $ others opts) $ withArgs ["--help"] $ void (cmdArgsRun options)
    runMultipleTests opts


runMultipleTests :: Options -> IO ()
runMultipleTests opt  = do
    putStrLn $ bold ++ "YATS " ++ yatsVersion ++ reset

    let srcs = filter isCppSource (others opt)
    let bins = filter isBinary (others opt)
    let copt = getOption (others opt)

    putStrLn $ "compiler options : " ++ show copt
    putStrLn $ "source code tests: " ++ show srcs
    putStrLn $ "binary tests     : " ++ show bins

    t1 <- forM bins $ runYatsBinTests opt >=> checkVerdict
    t2 <- forM srcs $ runYatsSrcTests opt copt >=> checkVerdict

    let total = length t1 + length t2

    let p1 = foldr (\b acc -> if b == verdictOk then acc + 1 else acc) 0 t1
    let p2 = foldr (\b acc -> if b == verdictOk then acc + 1 else acc) 0 t2

    if (p1+p2) == total
        then putStrLn $ bold ++ "Summary: all tests successfully passed." ++ reset
        else do putStrLn $ bold ++ "Summary: " ++ show (p1+p2) ++ " tests passed out of " ++ show total  ++ ":" ++ reset
                mapM_ (\(name, msg) -> do
                        el <- getExceptions $ "/tmp/" ++ name
                        putStrLn $ "    " ++ name ++ ": " ++  msg ++
                            if null el
                                then "!"
                                else "\n    -> exceptions: " ++ intercalate "    \n" el
                      ) $
                      mapMaybe (\(e,n) -> if e /= verdictOk then Just (n, getErrorString e) else Nothing) (zip (t1 ++ t2) (bins ++ srcs))


runYatsBinTests :: Options -> FilePath -> IO TestVerdict
runYatsBinTests opt bin = do
    putStrLn $ bold ++ "Running " ++ bin ++ "..." ++ reset
    liftM (ExitSuccess, ExitSuccess,) $ runBinary opt bin


runYatsSrcTests :: Options -> [CompOpt] -> Source -> IO TestVerdict
runYatsSrcTests opt copt src = do
    se <- countStaticErrors src
    b1 <- liftM (all (== ExitSuccess)) $ mapM (runStaticTest opt src copt se) $ take se [0..]
    (b2,b3) <- runtimeSrcTest opt src copt
    return (if b1 then ExitSuccess else ExitFailure 1,b2,b3)


runtimeSrcTest:: Options -> Source -> [CompOpt] -> IO (ExitCode, ExitCode)
runtimeSrcTest opt src copt = liftM makeCmd getCompiler >>= \cmd -> liftM2 (,) (system cmd) (runBinary opt (src ++ ".out"))
    where makeCmd cxx = compilerCmd cxx src ++ " " ++ unwords copt ++ " 2> /dev/null"


runStaticTest :: Options -> Source -> [CompOpt] -> Int ->  Int -> IO ExitCode
runStaticTest opt src copt total n = do
    putStrLn $ bold ++ "Running " ++ show (n+1) ++ " out of " ++ show total ++ " static assert tests on " ++ src ++ "..." ++ reset
    r <- liftM makeCmd getCompiler >>= system
    if r == ExitSuccess
      then runBinary opt $ src ++ ".out"
      else return ExitSuccess
        where makeCmd cxx = compilerCmd cxx src ++ " " ++ unwords copt ++ " -DYATS_STATIC_ERROR=" ++ show n ++ " 2> /dev/null"


checkVerdict :: TestVerdict -> IO TestVerdict
checkVerdict ret@(ExitSuccess, ExitSuccess, ExitSuccess)   = return ret
checkVerdict ret@(ExitFailure _ , _ ,  _     )             = putStrLn ( blue  ++ bold ++ "Test failed: " ++ getErrorString ret ++ reset ) >> return ret
checkVerdict ret@(ExitSuccess, ExitFailure _ , _     )     = putStrLn ( green ++ bold ++ "Test failed: " ++ getErrorString ret ++ reset ) >> return ret
checkVerdict ret@(ExitSuccess, ExitSuccess, ExitFailure _) = putStrLn ( red   ++ bold ++ "Test failed: " ++ getErrorString ret ++ reset ) >> return ret


getErrorString :: TestVerdict -> String
getErrorString (ExitSuccess, ExitSuccess, ExitSuccess)      = "OK"
getErrorString (ExitFailure n , _ , _)                      = "static assert error [exit code = " ++ show n ++ "]"
getErrorString (ExitSuccess   , ExitFailure n , _ )         = "compiler error [exit code = " ++ show n ++ "]"
getErrorString (ExitSuccess   , ExitSuccess, ExitFailure n) = "runtime error [exit code = " ++ show n ++ "]"


getExceptions :: FilePath -> IO [String]
getExceptions name = doesFileExist name >>= \f ->
    if f then liftM lines $ readFile name
         else return []


runBinary :: Options -> FilePath -> IO ExitCode
runBinary opt name =
    system (case () of
                _ | "./" `isPrefixOf` name -> name ++ optarg
                  | "/" `isPrefixOf`  name -> name ++ optarg
                  | otherwise  -> "./" ++ name ++ optarg)
    where optarg = if verbose opt then " -v -s " else " -s "


countStaticErrors :: Source -> IO Int
countStaticErrors file =
    liftM (length . filter (beginWith "StaticError") . lines) $ readFile file


getCompiler :: IO Compiler
getCompiler =
    fileExist "/usr/bin/g++" >>= (\b -> return $ if b then Gcc else Clang)


getCompilerExe :: Compiler -> String
getCompilerExe Gcc   = "/usr/bin/g++"
getCompilerExe Clang = "/usr/bin/clang++"


getCompilerOpt :: Compiler -> [String]
getCompilerOpt Gcc   =  [ "-std=c++11", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt Clang =  [ "-std=c++1y", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" , "-Wno-unneeded-internal-declaration"]


compilerCmd :: Compiler -> String -> String
compilerCmd cxx src = getCompilerExe cxx ++ " " ++ unwords (getCompilerOpt cxx) ++ " " ++ src ++ " -o " ++ src ++ ".out"


beginWith :: String -> String -> Bool
beginWith ys xs = ys `isPrefixOf` dropWhile isSpace xs


getOption :: [String] -> [String]
getOption = filter (not . isBinary) . filter (not . isCppSource)


isCppSource :: String -> Bool
isCppSource name = any (`isSuffixOf` name) [".cpp", ".CPP", ".cxx", ".cc"]


isBinary :: FilePath -> Bool
isBinary name = unsafePerformIO $
    doesFileExist name >>= \b ->
        if b then getFileStatus name >>= \status -> return $ intersectFileModes (fileMode status) ownerExecuteMode == ownerExecuteMode
             else return False

