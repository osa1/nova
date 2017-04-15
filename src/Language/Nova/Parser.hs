{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Nova.Parser where

----------------------------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import Prelude hiding (lex)
import Text.Megaparsec hiding (State, runParser, runParser', token)
import qualified Text.Megaparsec as P
----------------------------------------------------------------------------------------------------
import Language.Nova.Lexer
----------------------------------------------------------------------------------------------------

instance Stream [Loc Tok] where
    type Token [Loc Tok] = Loc Tok
    uncons = Data.List.uncons
    updatePos _ _ _ (L p1 _ p2) = (p1, p2)

type Parser = ParsecT Dec [Loc Tok] (State (NonEmpty Word))

runParser :: Parser a -> String -> Either String a
runParser p s = case lex s of
                  Left (ParseError pos _ _ _) -> Left ("Parse error at: " ++ show pos)
                  Right toks -> runParser' p toks

runParser' :: Parser a -> [Loc Tok] -> Either String a
runParser' p toks =
    case evalState (runParserT p "" toks) (1 :| []) of
      Left (ParseError pos _ _ _) -> Left ("Parse error at: " ++ show pos)
      Right ret -> return ret

pushNextIndentation :: Parser ()
pushNextIndentation = do
    L (SourcePos _ _ new) _ _ <- try (lookAhead (P.token Right Nothing))
    modify (NE.cons (unPos new))

popIndentation :: Parser Word
popIndentation =
    get >>= \case
      _ :| [] -> fail "popIndentation: Can't pop top-level"
      i :| (i' : rest) -> do
        put (i' :| rest)
        return i

assertIndentation :: (Word -> Word -> Bool) -> Parser ()
assertIndentation rel = do
    L (SourcePos _ _ new) t _ <- try (lookAhead (P.token Right Nothing))
    cur :| _ <- get
    unless (unPos new `rel` cur) (fail ("Indentation error " ++ show cur ++ " " ++ show t ++ "-" ++ show new))

----------------------------------------------------------------------------------------------------
-- * AST

data Decl
  = FunD FunDecl
  | IncludeD T.Text
  deriving (Show)

data FunDecl = FunDecl Ident [(Ident, Type)] Type [Stmt]
  deriving (Show)

data Type
  = Type    Ident
  | TypeApp Ident [Type]
  deriving (Show)

data Stmt
  = FunCallS FunCall
  deriving (Show)

data Expr
  = FunCallE FunCall
  | StringE  T.Text
  | IdentE Ident
  deriving (Show)

data FunCall = FunCall Expr [Expr]
  deriving (Show)

----------------------------------------------------------------------------------------------------

parseModule :: Parser [Decl]
parseModule = many (assertIndentation (==) >> parseDecl) <* eof

parseType :: Parser Type
parseType = do
    ty1 <- ident
    optional (token LAngle) >>= \case
      Nothing -> return (Type ty1)
      Just _  -> do
        tys <- sepBy parseType (token Comma)
        token_ RAngle
        return (TypeApp ty1 tys)

parseDecl :: Parser Decl
parseDecl = choice
    [ parseInclude
    , FunD <$> parseFunDecl
    ]

parseInclude :: Parser Decl
parseInclude = IncludeD <$> (token_ Include >> stringLit)

parseFunDecl :: Parser FunDecl
parseFunDecl = do
    token_ Fn
    f_name <- ident
    token_ LParen
    params <- parseParams
    token_ RParen
    token_ Colon
    ret_ty <- parseType
    token_ Assign
    assertIndentation (>)
    pushNextIndentation
    stmts <- many (assertIndentation (==) >> parseStmt)
    _ <- popIndentation
    return (FunDecl f_name params ret_ty stmts)

parseParams :: Parser [(Ident, Type)]
parseParams = param `sepBy` token Comma
  where
    param = do
      i <- ident
      token_ Colon
      ty <- parseType
      return (i, ty)

parseStmt :: Parser Stmt
parseStmt = choice
    [ FunCallS <$> parseFunCall ]

parseFunCall :: Parser FunCall
parseFunCall = do
    fn <- expr
    token_ LParen
    args <- expr `sepBy` token Comma
    token_ RParen
    return (FunCall fn args)

expr :: Parser Expr
expr =
    -- FIXME only parses identifiers and strings for now, no function calls
    choice [ StringE <$> stringLit
           , IdentE <$> ident
           ]

token :: Tok -> Parser (Loc Tok)
token t = P.token testTok Nothing
  where
    testTok x =
      if unLoc x == t
        then Right x
        else Left (S.singleton (Tokens (x :| [])), S.empty, S.empty)

token_ :: Tok -> Parser ()
token_ = void . token

ident :: Parser Ident
ident = P.token testTok Nothing
  where
    testTok (L _ (Id i) _) = Right i
    testTok x = Left (S.singleton (Tokens (x:|[])), S.empty, S.empty)

stringLit :: Parser T.Text
stringLit = P.token testTok Nothing
  where
    testTok (L _ (StringLit str) _) = Right str
    testTok x = Left (S.singleton (Tokens (x:|[])), S.empty, S.empty)
