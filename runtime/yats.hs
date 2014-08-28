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

module Main where

import System.Environment(getArgs)
import System.Process
import System.Exit
import System.Posix.Files
import System.IO.Unsafe
import System.Directory
import System.Console.ANSI

import Control.Monad

import Data.List
import Data.Char
import Data.Maybe

type Option = String
type Source = String

data Compiler = Gcc | Clang deriving (Show, Eq, Ord)


yatsVersion :: String
yatsVersion = "runtime v1.1"

magenta = setSGRCode [SetColor Foreground Vivid Magenta]
blue    = setSGRCode [SetColor Foreground Vivid Blue]
cyan    = setSGRCode [SetColor Foreground Vivid Cyan]
green   = setSGRCode [SetColor Foreground Vivid Green]
red     = setSGRCode [SetColor Foreground Vivid Red]
bold    = setSGRCode [SetConsoleIntensity BoldIntensity]
reset   = setSGRCode []


main :: IO ()
main = do
    args <- getArgs
    if null args
        then putStrLn "yats: binary1 binary2 source1.cpp [source2.cpp...] [-comp_opt -comp_opt2... ]"
        else runMultipleTests args


runMultipleTests :: [String] -> IO ()
runMultipleTests xs = do
    putStrLn $ "YATS " ++ yatsVersion

    let srcs = filter isCppSource xs
    let bins = filter isBinary xs
    let opts = getOption xs

    putStrLn $ "compiler options : " ++ show opts
    putStrLn $ "source code tests: " ++ show srcs
    putStrLn $ "binary tests     : " ++ show bins

    t1 <- forM bins $ runYatsBinTests >=> checkError
    t2 <- forM srcs $ runYatsSrcTests opts >=> checkError

    let total = length t1 + length t2

    let p1 = foldr (\b acc -> if b then acc + 1 else acc) 0 t1
    let p2 = foldr (\b acc -> if b then acc + 1 else acc) 0 t2

    if (p1+p2) == total
        then putStrLn $ bold ++ "All tests successfully passed." ++ reset
        else do putStrLn $ bold ++ show (p1+p2) ++ " tests passed out of " ++ show total  ++ ":" ++ reset
                mapM_ (\name -> putStrLn $ "    " ++ name ++ " failed!") $ mapMaybe (\(e,n) -> if not e then Just n else Nothing) (zip (t1 ++ t2) (bins ++ srcs))


runYatsBinTests :: FilePath -> IO (Bool, Bool, Bool)
runYatsBinTests bin = do
    putStrLn $ bold ++ "Running " ++ bin ++ "..." ++ reset
    liftM (True, True,) $ liftM (==ExitSuccess) (runBinary bin)


runYatsSrcTests :: [Option] -> Source -> IO (Bool,Bool,Bool)
runYatsSrcTests opt src = do
    se <- countStaticErrors src
    putStrLn $ bold ++ "Running " ++ show se ++ " static checks on " ++ src ++ "..." ++ reset
    b1 <- liftM (all (== ExitSuccess)) (mapM (runStaticTest src opt) $ take se [0..])
    (b2,b3) <- liftM (\(a,b) -> (a == ExitSuccess, b == ExitSuccess)) $ runtimeSrcTest src opt
    return (b1,b2,b3)


runtimeSrcTest:: Source -> [Option] -> IO (ExitCode, ExitCode)
runtimeSrcTest src opt = liftM makeCmd getCompiler >>= \cmd -> liftM2 (,) (system cmd) (runBinary (src ++ ".out"))
    where makeCmd cxx = compilerCmd cxx src ++ unwords opt ++ " 2> /dev/null"


runStaticTest :: Source -> [Option] -> Int -> IO ExitCode
runStaticTest src opt n = do
    r <- liftM makeCmd getCompiler >>= system
    if r == ExitSuccess
      then runBinary $ src ++ ".out"
      else return ExitSuccess
        where makeCmd cxx = compilerCmd cxx src ++ unwords opt ++ " -DYATS_STATIC_ERROR=" ++ show n ++ " 2> /dev/null"


checkError :: (Bool, Bool, Bool) -> IO Bool
checkError (True, True, True   ) = return True
checkError (False, _ ,   _     ) = putStrLn ( blue  ++ bold ++ "Test failed: static assert error!" ++ reset ) >> return False
checkError (True, False, _     ) = putStrLn ( green ++ bold ++ "Test failed: compiler error!"      ++ reset ) >> return False
checkError (True, True , False ) = putStrLn ( red   ++ bold ++ "Test failed: run-time error!"      ++ reset ) >> return False


runBinary :: FilePath -> IO ExitCode
runBinary name = system (case () of
                          _ | "./" `isPrefixOf` name -> name
                            | "/" `isPrefixOf` name -> name
                            | otherwise  -> "./" ++ name ++ " -v")


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

