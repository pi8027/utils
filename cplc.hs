
module Main where

import Safe
import Data.Maybe
import Data.Either
import Data.List
import Control.Monad
import Control.Monad.Instances

import System.Environment

main :: IO ()
main = do
    (files, modes) <- liftM (partitionEithers . map parseArg) getArgs
    let expandtab '\t' = replicate (fromMaybe 4 (getMode "t" modes)) ' '
        expandtab c = [c]
        check = not . null .
            drop (fromMaybe 80 (getMode "c" modes)) . concatMap expandtab
    contents <- forM files $ liftM2 liftM (,) readFile
    sequence_ [putStrLn $ path ++ ":" ++ show n ++ ": " ++ l |
        (path, c) <- contents, (n, l) <- zip [1..] $ lines c, check l]

parseArg :: String -> Either String (String, String)
parseArg ('-' : str) =
    let (name, body) = break ('=' ==) str in Right (name, tailSafe body)
parseArg str = Left str

getMode :: Read a => String -> [(String, String)] -> Maybe a
getMode tag = lookup tag >=> liftM fst . find (null . snd) . reads

