module HLox.Lexer ( Token(..)
                  , Keyword(..)
                  , SourcePos(..)
                  , Located(..)
                  , Lexeme(..)
                  , CompilerError(..)
                  , lex
                  ) where

import Prelude hiding (True, False, null, head, take, tail, lex)
import qualified Prelude
import Data.Bool (bool)
import Data.Char (isAlpha, isDigit, isSpace)

import Control.Monad.Trans.RWS (RWS, execRWS, tell, get, gets, put)

import Data.Text (Text, null, head, take, tail, pack, unpack, toLower, uncons, singleton)

import Data.Bifunctor (bimap)
import Data.Either.Validation (Validation(..), validationToEither)

import qualified Data.Map.Strict as M

data Token = LeftParen
           | RightParen
           | LeftBrace
           | RightBrace
           | Comma
           | Dot
           | Minus
           | Plus
           | Semicolon
           | Slash
           | Star
           | Bang
           | BangEqual
           | Equal
           | EqualEqual
           | Greater
           | GreaterEqual
           | Less
           | LessEqual
           | Identifier Text
           | StringLit Text
           | Number Double
           | Keyword Keyword
           | Eof
           deriving (Eq, Ord, Show)

-- Only covers simple 1- or 2-character operators. Intentionally omitted are:
-- Slash: because comments require special handling
-- Identifier, StringLit, Number, Keyword, and Eof: they aren't operators
operators :: M.Map Text Token
operators = M.fromList [ ("(", LeftParen)
                       , (")", RightParen)
                       , ("{", LeftBrace)
                       , ("}", RightBrace)
                       , (",", Comma)
                       , (".", Dot)
                       , ("-", Minus)
                       , ("+", Plus)
                       , (";", Semicolon)
                       -- Slash omitted
                       , ("*", Star)
                       , ("!", Bang)
                       , ("!=", BangEqual)
                       , ("=", Equal)
                       , ("==", EqualEqual)
                       , ("<", Less)
                       , ("<=", LessEqual)
                       -- Omitted non-operator tokens
                       ]

data Keyword = And
             | Class
             | Else
             | False
             | Fun
             | For
             | If
             | Nil
             | Or
             | Print
             | Return
             | Super
             | This
             | True
             | Var
             | While
             deriving (Eq, Ord, Show, Enum, Bounded)

keywords :: M.Map Text Keyword
keywords = M.fromList $ do
  k <- [minBound .. maxBound]
  pure (toLower . pack . show $ k, k)

data SourcePos = SourcePos { lineNumber :: Int } deriving (Eq, Ord, Show)
data Located a = Located SourcePos a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
data Lexeme = Lexeme { token :: Token, text :: Text } deriving (Eq, Ord, Show)
data CompilerError = CompilerError Text deriving (Eq, Show)

lex :: Text -> Either [Located CompilerError] [Located Lexeme]
lex = validationToEither . sequenceA .  fmap lowerLocations . lex'
  where lowerLocations (Located pos result) = bimap lowerError lowerLexeme result
          where lowerError err = [Located pos err]
                lowerLexeme lexeme = Located pos lexeme

data LexerState = LS { startOfToken, remaining :: Text
                     , tokenLen, lineNum :: Int
                     }
type LexerM a = RWS () [Located (Validation CompilerError Lexeme)] LexerState a

lex' :: Text -> [Located (Validation CompilerError Lexeme)]
lex' src = snd . execRWS go () $ LS src src 0 1
  where go :: LexerM ()
        go = advance >>= \case
          Nothing -> pure ()
          Just x -> case x of
            '(' -> emit LeftParen
            ')' -> emit RightParen
            '{' -> emit LeftBrace
            '}' -> emit RightBrace
            ',' -> emit Comma
            '.' -> emit Dot
            '-' -> emit Minus
            '+' -> emit Plus
            ';' -> emit Semicolon
            '*' -> emit Star
            '!' -> Bang `followedByEqualIs` BangEqual
            '=' -> Equal `followedByEqualIs` EqualEqual
            '<' -> Less `followedByEqualIs` LessEqual
            '>' -> Greater `followedByEqualIs` GreaterEqual
            '/' -> match '/' >>= bool (emit Slash) comment
            '"' -> string
            c | isSpace c -> reset
              | isDigit c -> number
              | isAlpha c -> identifier
              | otherwise -> report $ "Unexpected character: '" <> singleton c <> "'."
        reset :: LexerM ()
        reset = do
          LS _ curr _ line <- get
          put $ LS curr curr 0 line
          go
        consumedText :: LexerM Text
        consumedText = do
          LS start _ len _ <- get
          pure $ take len start
        emit :: Token -> LexerM ()
        emit tok = do
          LS start _curr len line <- get
          tell [Located (SourcePos line) (Success (Lexeme tok (take len start)))]
          reset
        report :: Text -> LexerM ()
        report msg = do
          line <- gets lineNum
          tell [Located (SourcePos line) (Failure (CompilerError msg))]
          reset
        advance :: LexerM (Maybe Char)
        advance = do
          LS start curr len line <- get
          case uncons curr of
            Nothing -> pure Nothing
            Just (c, t) -> Just c <$ put (LS start t (len + 1)
                                             (if c == '\n' then line + 1 else line))
        peek :: LexerM (Maybe Char)
        peek = fmap fst . uncons <$> gets remaining
        satisfy :: (Char -> Bool) -> LexerM Bool
        satisfy p = peek >>= \case
          Just actual | p actual -> Prelude.True <$ advance
          _ -> pure Prelude.False
        match :: Char -> LexerM Bool
        match expected = satisfy (== expected)
        followedByEqualIs :: Token -> Token -> LexerM ()
        short `followedByEqualIs` long = match '=' >>= emit . bool short long
        many :: (Char -> Bool) -> LexerM ()
        many p = satisfy p >>= bool (pure ()) (many p)
        comment :: LexerM ()
        comment = many (/= '\n') *> advance *> reset
        string :: LexerM ()
        string = (many (/= '"') *> advance) >>= \case
          Just '"' -> do
            LS start _curr len _line <- get
            emit (StringLit (take (len - 2) . tail $ start))
          _ -> report "Unterminated string."
        number :: LexerM ()
        number = do
          many isDigit
          t <- gets remaining
          case uncons t of
            Just ('.', t') | not (null t') && isDigit (head t') ->
                             advance *> many isDigit *> emitNumber
            _ -> emitNumber
        emitNumber :: LexerM ()
        emitNumber = do
          num <- read . unpack <$> consumedText
          emit (Number num)
        identifier :: LexerM ()
        identifier = do
          many isAlpha
          tok <- consumedText
          emit $ maybe (Identifier tok) Keyword (M.lookup tok keywords)
