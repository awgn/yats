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

import System(getArgs)
import System.Process
import System.Exit
import System.Directory

import Data.List
import Data.Char

type Option = String
type Source = String

yatsVersion = "runtime v0.1" 


main :: IO ()
main = do
    args <- getArgs
    if (args == [])
    then     
        putStrLn "yats: source-test.cpp [source-test2.cpp...] [-comp_opt -comp_opt2... ]"
    else do
        r <- runMultipleTests args
        return ()

runMultipleTests :: [String] -> IO [(Bool,Bool)]
runMultipleTests xs = do                
                    putStrLn $ "YAST " ++ yatsVersion
                    let srcs = getSource xs
                    let opt = getOption xs
                    mapM (runYatsTests opt) srcs


runYatsTests :: [Option] -> Source -> IO (Bool,Bool)
runYatsTests opt src = do
                        se <- countStaticErrors src
                        putStrLn $ "Running " ++ show(se) ++ " static checks on " ++ src ++ "."
                        r <- mapM (runStaticTest src opt) $ take se [0..]
                        r' <- runtimeTest src opt
                        return ((r' == ExitSuccess), (and $ map (\x -> if x == ExitSuccess then True else False) r)  
                                 )

runtimeTest:: Source -> [Option] -> IO ExitCode
runtimeTest src opt = do 
                        let cmd = "/usr/bin/g++ -std=c++0x -O0 -I /usr/local/include -w " ++ src ++ " -o " ++ src ++ ".out " 
                                    ++ unwords(opt) ++ " 2> /dev/null"
                        r <- system cmd >> system ("./" ++ src ++ ".out")
                        removeFile (src ++ ".out")
                        return r

runStaticTest :: Source -> [Option] -> Int -> IO ExitCode
runStaticTest src opt n = do
                        let cmd = "/usr/bin/g++ -std=c++0x -O0 -I /usr/local/include -w " ++ src ++ " -o " ++ src ++ ".out " 
                                    ++ unwords(opt) ++ " -DYATS_STATIC_ERROR=" ++ show(n) ++ " 2> /dev/null"
                        -- putStrLn $ "   " ++ cmd 
                        r <- system cmd
                        r'<- (if (r == ExitSuccess) 
                                then do
                                    r'' <- system ("./" ++ src ++ ".out")
                                    removeFile (src ++ ".out")
                                    return r''
                                else 
                                    return ExitSuccess)
                        return r'

countStaticErrors :: Source -> IO Int
countStaticErrors file = do
                        src <- readFile file
                        return $ length $ filter (beginWith "StaticError") (lines src)

beginWith :: String -> String -> Bool
beginWith _ [] = False
beginWith [] _ = False
beginWith (y:ys) (x:xs)
    | isSpace x = beginWith (y:ys)  xs
    | (x == y) && (isPrefixOf ys xs) = True
    | otherwise = False


getSource :: [String] -> [String]
getSource = filter isCppSource


getOption :: [String] -> [String]
getOption = filter (not . isCppSource)


isCppSource :: String -> Bool
isCppSource name =  isSuffixOf ".cpp" name || isSuffixOf ".cc" name



