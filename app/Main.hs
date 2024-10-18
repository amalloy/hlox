module Main (main) where

import Prelude hiding (unlines, readFile, getLine, putStr)

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (stdout, stderr, isEOF, hFlush)

import Data.Text (Text, pack, unlines)
import Data.Text.IO (putStr, hPutStrLn, readFile, getLine)

import Control.Monad (when)

import qualified HLox.Lexer as Lex

main :: IO ()
main = getArgs >>= \case
  [] -> repl
  [fileName] -> putStr . run =<< readFile fileName
  _ -> do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " <> pack prog <> " [filename]"
    exitWith (ExitFailure 64) -- EX_USAGE

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  done <- isEOF
  when (not done) $ do
    src <- getLine
    putStr (run src)
    repl

-- each token ends with a newline
run :: Text -> Text
run src = unlines $ case Lex.lex src of
  Left errors -> do
    Lex.Located line (Lex.CompilerError msg) <- errors
    pure $ "[line " <> pack (show line) <>"] Error: " <> msg
  Right lexemes -> do
    Lex.Located _line (Lex.Lexeme tok _txt) <- lexemes
    pure $ pack (show tok)
