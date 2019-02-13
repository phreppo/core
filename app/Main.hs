module Main where

import Lib

import System.IO
import Language
import BaseParser
import CoreParser

readF :: String -> IO String
readF fileName = do 
    inh <- openFile fileName ReadMode
    prog <- readloop inh
    hClose inh
    return prog

readloop :: Handle -> IO [Char]
readloop inh = do 
    ineof <- hIsEOF inh
    if ineof
        then return []
        else do
            x <- hGetLine inh
            xs <- readloop inh
            return (x ++ xs)

comp :: [(CoreProgram, Name)] -> CoreProgram
comp []         = error "Parse error"
comp [(e,[])]   = e
comp [(_,rest)] = error ("doesn't use all input: " ++ rest)

main :: IO (Program Name)
main = do 
    putStr "> Insert program source file name: "
    hFlush stdout
    programName <- getLine
    inp <- readF programName
    print $ comp $ parse parseProg inp
    return (comp (parse parseProg inp))

p = parse parseExpr