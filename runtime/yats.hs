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

module Main where

import System.Environment(getArgs)
import System.Process
import System.Exit
import System.Posix.Files

import Control.Monad

import Data.List
import Data.Char

type Option = String
type Source = String

data Compiler = Gcc | Clang 
                    deriving (Show, Eq, Ord)

yatsVersion :: String
yatsVersion = "runtime v1.0" 


main :: IO ()
main = do
    args <- getArgs
    if getSource args == []
        then putStrLn "yats: source-test.cpp [source-test2.cpp...] [-comp_opt -comp_opt2... ]"
        else void (runMultipleTests args)


runMultipleTests :: [String] -> IO [(Bool,Bool)]
runMultipleTests xs = do 
    putStrLn $ "YATS " ++ yatsVersion
    let srcs = getSource xs
    let opt  = getOption xs
    mapM (runYatsTests opt) srcs


runYatsTests :: [Option] -> Source -> IO (Bool,Bool)
runYatsTests opt src = do
    se <- countStaticErrors src
    putStrLn $ "Running " ++ show se ++ " static checks on " ++ src ++ "."
    liftM2 (,) (liftM (all (== ExitSuccess)) (mapM (runStaticTest src opt) $ take se [0..]))  
               (liftM (==ExitSuccess) (runtimeTest src opt))


runtimeTest:: Source -> [Option] -> IO ExitCode
runtimeTest src opt = liftM makeCmd getCompiler >>= system >> system ("./" ++ src ++ ".out") 
                        where makeCmd cxx = compilerCmd cxx src ++ unwords opt ++ " 2> /dev/null" 


runStaticTest :: Source -> [Option] -> Int -> IO ExitCode
runStaticTest src opt n = do
    r <- liftM makeCmd getCompiler >>= system
    if r == ExitSuccess 
      then system $ "./" ++ src ++ ".out" 
      else return ExitSuccess
        where makeCmd cxx = compilerCmd cxx src ++ unwords opt ++ " -DYATS_STATIC_ERROR=" ++ show n ++ " 2> /dev/null" 


countStaticErrors :: Source -> IO Int
countStaticErrors file = liftM (length . filter (beginWith "StaticError") . lines) $ readFile file 


getCompiler :: IO Compiler
getCompiler =  fileExist "/usr/bin/clang++" >>= (\b -> if b then return Clang else return Gcc)


getCompilerExe :: Compiler -> String
getCompilerExe Gcc   = "/usr/bin/g++"
getCompilerExe Clang = "/usr/bin/clang++"


getCompilerOpt :: Compiler -> [String]
getCompilerOpt Gcc   =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" ]
getCompilerOpt Clang =  [ "-std=c++0x", "-O0", "-D_GLIBCXX_DEBUG", "-Wall", "-Wextra", "-Wno-unused-parameter" , "-Wno-unneeded-internal-declaration"]


compilerCmd :: Compiler -> String -> String
compilerCmd cxx src = getCompilerExe cxx ++ " " ++ unwords (getCompilerOpt cxx) ++ " " ++ src ++ " -o " ++ src ++ ".out "
                                    

beginWith :: String -> String -> Bool
beginWith ys xs = ys `isPrefixOf` dropWhile isSpace xs


getSource :: [String] -> [String]
getSource = filter isCppSource


getOption :: [String] -> [String]
getOption = filter (not . isCppSource)


isCppSource :: String -> Bool
isCppSource name =  ".cpp" `isSuffixOf` name || ".cc" `isSuffixOf` name


