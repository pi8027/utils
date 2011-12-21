
module Main where

import Prelude hiding (catch)
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Exception hiding (bracket)
import System.IO
import System.Exit

import Agda.TypeChecking.Monad.Base
import qualified Agda.Interaction.GhciTop as Agda
import qualified Agda.Interaction.BasicOps as Agda.B

-- ReadS Parser Combinator

newtype Parser a = Parser { runParser :: ReadS a }

instance Functor Parser where
    fmap f p = Parser (\s -> map (first f) (runParser p s))

instance Applicative Parser where
    pure a = Parser (\s -> [(a, s)])
    a <*> b = Parser (runParser a >=> \(f, s) -> map (first f) (runParser b s))

instance Alternative Parser where
    empty = Parser (const [])
    a <|> b = Parser (\s -> runParser a s ++ runParser b s)

instance Monad Parser where
    return a = pure a
    p >>= f = Parser $ runParser p >=> uncurry (runParser . f)

spaces :: Parser ()
spaces = Parser readsSpaces where
    readsSpaces :: ReadS ()
    readsSpaces (x : xs) | isSpace x = readsSpaces xs
    readsSpaces s = [((), s)]

string :: String -> Parser ()
string s = spaces >> Parser (readsStr s) where
    readsStr :: String -> ReadS ()
    readsStr [] s = [((), s)]
    readsStr (x : xs) (y : ys) | x == y = readsStr xs ys
    readsStr _ _ = []

readsP :: Read a => Parser a
readsP = Parser reads

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 sep p = p >>= \r -> return [r] <|> (sep >> (r :) <$> sepBy1 sep p)

sepBy0 :: Parser () -> Parser a -> Parser [a]
sepBy0 sep p = return [] <|> sepBy1 sep p

listP :: Parser a -> Parser [a]
listP p = string "[" *> sepBy0 (string ",") p <* string "]"

maybeP :: Parser a -> Parser (Maybe a)
maybeP p =
    (Nothing <$ string "Nothing") <|>
    bracket (string "Just" >> Just <$> p)

bracket :: Parser a -> Parser a
bracket p = string "(" *> p <* string ")"

-- Commands

goalCommand' ::
    Parser a -> String -> (a -> Agda.GoalCommand) -> Parser Agda.Interaction
goalCommand' p str f = do
    string str
    liftM4 f p (fromIntegral <$> readsP) rangeP readsP

goalCommand :: String -> Agda.GoalCommand -> Parser Agda.Interaction
goalCommand str f = goalCommand' (return ()) str (const f)

cmdLoad :: Parser Agda.Interaction
cmdLoad = string "cmd_load" >> liftM2 Agda.cmd_load readsP readsP

cmdCompile :: Parser Agda.Interaction
cmdCompile = do
    string "cmd_compile"
    liftM3 Agda.cmd_compile backend readsP readsP
    where
    backend =
      Agda.MAlonzo <$ string "MAlonzo" <|>
      Agda.Epic <$ string "Epic" <|>
      Agda.JS <$ string "JS"

cmdConstraints :: Parser Agda.Interaction
cmdConstraints = Agda.cmd_constraints <$ string "cmd_constraints"

cmdMetas :: Parser Agda.Interaction
cmdMetas = Agda.cmd_metas <$ string "cmd_metas"

rangeP :: Parser Agda.Range
rangeP =
    bracket (string "Range" >> Agda.Range <$> listP intervalP) <|>
    (Agda.noRange <$ string "noRange")
    where
    intervalP = do
        string "Interval"
        liftM2 Agda.Interval (bracket positionP) (bracket positionP)
    positionP = do
        string "Pn"
        liftM4 Agda.Pn
            (maybeP (bracket
                (string "mkAbsolute" >> Agda.mkAbsolute <$> readsP)))
            readsP readsP readsP

cmdGive :: Parser Agda.Interaction
cmdGive = goalCommand "cmd_give" Agda.cmd_give

cmdRefine :: Parser Agda.Interaction
cmdRefine = goalCommand "cmd_refine" Agda.cmd_refine

cmdIntro :: Parser Agda.Interaction
cmdIntro = goalCommand "cmd_intro" Agda.cmd_intro

cmdRefineOrIntro :: Parser Agda.Interaction
cmdRefineOrIntro = goalCommand "cmd_refine_or_intro" Agda.cmd_refine_or_intro

cmdAuto :: Parser Agda.Interaction
cmdAuto = goalCommand "cmd_auto" Agda.cmd_auto

rewriteP :: Parser Agda.B.Rewrite
rewriteP =
    (Agda.B.AsIs <$ string "Agda.Interaction.BasicOps.AsIs") <|>
    (Agda.B.Instantiated <$ string "Agda.Interaction.BasicOps.Instantiated") <|>
    (Agda.B.HeadNormal <$ string "Agda.Interaction.BasicOps.HeadNormal") <|>
    (Agda.B.Normalised <$ string "Agda.Interaction.BasicOps.Normalised")

cmdContext :: Parser Agda.Interaction
cmdContext = goalCommand' rewriteP "cmd_context" Agda.cmd_context

cmdInfer :: Parser Agda.Interaction
cmdInfer = goalCommand' rewriteP "cmd_infer" Agda.cmd_infer

cmdGoalType :: Parser Agda.Interaction
cmdGoalType = goalCommand' rewriteP "cmd_goal_type" Agda.cmd_goal_type

cmdGoalTypeContext :: Parser Agda.Interaction
cmdGoalTypeContext = goalCommand' rewriteP
    "cmd_goal_type_context" Agda.cmd_goal_type_context

cmdGoalTypeContextInfer :: Parser Agda.Interaction
cmdGoalTypeContextInfer = goalCommand' rewriteP
    "cmd_goal_type_context_infer" Agda.cmd_goal_type_context_infer

cmdShowModuleContents :: Parser Agda.Interaction
cmdShowModuleContents = goalCommand
    "cmd_show_module_contents" Agda.cmd_show_module_contents

cmdShowModuleContentsToplevel :: Parser Agda.Interaction
cmdShowModuleContentsToplevel = do
    string "cmd_show_module_contents_toplevel"
    Agda.cmd_show_module_contents_toplevel <$> readsP

cmdMakeCase :: Parser Agda.Interaction
cmdMakeCase = goalCommand "cmd_make_case" Agda.cmd_make_case

cmdSolveAll :: Parser Agda.Interaction
cmdSolveAll = Agda.cmd_solveAll <$ string "cmd_solveAll"

cmdCompute :: Parser Agda.Interaction
cmdCompute = goalCommand' readsP "cmd_compute" Agda.cmd_compute

cmdInferToplevel :: Parser Agda.Interaction
cmdInferToplevel = do
    string "cmd_infer_toplevel"
    liftM2 Agda.cmd_infer_toplevel rewriteP readsP

cmdComputeToplevel :: Parser Agda.Interaction
cmdComputeToplevel = do
    string "cmd_compute_toplevel"
    liftM2 Agda.cmd_compute_toplevel readsP readsP

cmdWriteHighlightingInfo :: Parser Agda.Interaction
cmdWriteHighlightingInfo = do
    string "cmd_write_highlighting_info"
    liftM2 Agda.cmd_write_highlighting_info readsP readsP

-- Input Parser

interactionParser :: Parser Agda.Interaction
interactionParser = bracket $
    cmdLoad <|> cmdCompile <|> cmdConstraints <|> cmdMetas <|>
    cmdGive <|> cmdRefine <|> cmdIntro <|> cmdRefineOrIntro <|> cmdAuto <|>
    cmdContext <|> cmdInfer <|> cmdGoalType <|>
    cmdGoalTypeContext <|> cmdGoalTypeContextInfer <|>
    cmdShowModuleContents <|> cmdShowModuleContentsToplevel <|>
    cmdMakeCase <|> cmdCompute <|> cmdInferToplevel <|> cmdComputeToplevel <|>
    cmdWriteHighlightingInfo

ioTcmParser :: Parser (IO ())
ioTcmParser =
    string "ioTCM" >> liftM3 Agda.ioTCM readsP readsP interactionParser

-- Interaction

agdaModeInteract :: IO ()
agdaModeInteract = do
    putStr "Agda> "
    hFlush stdout
    eof <- isEOF
    unless eof $ do
        s <- getLine
        case [r | (r, "") <- runParser (ioTcmParser <* spaces) s] of
            [] -> return ()
            a : _ -> catch a catcher
        agdaModeInteract
    where
    catcher :: ExitCode -> IO ()
    catcher code = putStrLn $ "*** Exception: " ++ show code

main :: IO ()
main = agdaModeInteract

