{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Nova.Parser where

----------------------------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.State
import Data.Functor
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
  | IfS Expr -- cond
        [Stmt] -- body
        [(Expr, [Stmt])] -- elseif blocks
        (Maybe [Stmt]) -- else block
  | ReturnS (Maybe Expr)
  deriving (Show)

data Expr
  = FunCallE FunCall
  | StringE T.Text
  | IdentE Ident
  | NumberE Number
  | SelectE Expr Ident
  | IndexE Expr Expr
  | Unop Unop Expr
  | Binop Binop Expr Expr
  deriving (Show)

data FunCall = FunCall Expr [Expr]
  deriving (Show)

data Unop
  = Neg
  | Not
  | AddrOf
  | Deref
  deriving (Show)

unopPrec :: Unop -> Word
unopPrec Neg    = 100
unopPrec Not    = 100
unopPrec AddrOf = 100
unopPrec Deref  = 100

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  deriving (Show)

binopPrec :: Binop -> (Word, Word)
binopPrec Add = (50, 60)
binopPrec Sub = (50, 60)
binopPrec Mul = (80, 70)
binopPrec Div = (80, 70)
binopPrec Rem = (80, 70)

----------------------------------------------------------------------------------------------------

decls :: Parser [Decl]
decls = many (assertIndentation (==) >> decl) <* eof

type_ :: Parser Type
type_ = do
    ty1 <- ident
    optional (token LAngle) >>= \case
      Nothing -> return (Type ty1)
      Just _  -> do
        tys <- sepBy type_ (token Comma)
        token_ RAngle
        return (TypeApp ty1 tys)

decl :: Parser Decl
decl = choice
    [ include
    , FunD <$> funDecl
    ]

include :: Parser Decl
include = IncludeD <$> (token_ Include >> stringLit)

funDecl :: Parser FunDecl
funDecl = do
    token_ Fn
    f_name <- ident
    token_ LParen
    ps <- params
    token_ RParen
    token_ Colon
    ret_ty <- type_
    token_ Assign
    assertIndentation (>)
    pushNextIndentation
    stmts <- many (assertIndentation (==) >> stmt)
    _ <- popIndentation
    return (FunDecl f_name ps ret_ty stmts)

params :: Parser [(Ident, Type)]
params = param `sepBy` token Comma
  where
    param = do
      i <- ident
      token_ Colon
      ty <- type_
      return (i, ty)

stmt :: Parser Stmt
stmt = choice
    [ FunCallS <$> funCall
    , ReturnS <$> (token_ Return >> optional (assertIndentation (>) >> expr))
    , ifStmt
    ]

ifStmt :: Parser Stmt
ifStmt = do
    token_ If
    cond <- expr
    token_ Colon
    assertIndentation (>)
    pushNextIndentation
    stmts <- many (assertIndentation (==) >> stmt)
    _ <- popIndentation
    elseifs <- many elseif
    else_ <- optional else_block
    return (IfS cond stmts elseifs else_)
  where
    elseif :: Parser (Expr, [Stmt])
    elseif = do
      token_ ElseIf
      cond <- expr
      token_ Colon
      assertIndentation (>)
      pushNextIndentation
      stmts <- many (assertIndentation (==) >> stmt)
      _ <- popIndentation
      return (cond, stmts)

    else_block :: Parser [Stmt]
    else_block = do
      token_ Else
      token_ Colon
      assertIndentation (>)
      pushNextIndentation
      stmts <- many (assertIndentation (==) >> stmt)
      _ <- popIndentation
      return stmts

funCall :: Parser FunCall
funCall = expr >>= \case
    FunCallE funcall -> return funcall
    err -> fail ("unexpected " ++ show err)

expr :: Parser Expr
expr = fst <$> expr_prec 0

expr_prec :: Word -> Parser (Expr, Maybe Binop)
expr_prec limit0 = do
    (e1, bop) <- optional unop >>= \case
                   Nothing -> (, Nothing) <$> simple_expr
                   Just op -> do
                     (e1, bop) <- expr_prec (unopPrec op)
                     return (Unop op e1, bop)
    maybe (optional binop) (return . Just) bop >>= loop limit0 e1
  where
    loop :: Word -> Expr -> Maybe Binop -> Parser (Expr, Maybe Binop)
    loop _ e1 Nothing = return (e1, Nothing)
    loop cur_prec e1 (Just bop)
      | fst (binopPrec bop) > cur_prec = do
          (e2, nextOp) <- expr_prec (snd (binopPrec bop))
          loop cur_prec (Binop bop e1 e2) nextOp
      | otherwise = return (e1, Just bop)

simple_expr :: Parser Expr
simple_expr =
    choice [ StringE <$> stringLit
           , NumberE <$> numberLit
           , suffixExpToExp <$> suffixExp
           ]

data ExpSuffix
  = SFunCall [Expr]
  | SIndex   Expr
  | SSelect  Ident

data PrimaryExp
  = PIdent Ident
  | PParen Expr

data SuffixExp
  = SuffixExp PrimaryExp [ExpSuffix]

suffixExpToExp :: SuffixExp -> Expr
suffixExpToExp (SuffixExp pe ss) = foldl' foldSE (primaryExpToExpr pe) ss
  where
    foldSE :: Expr -> ExpSuffix -> Expr
    foldSE e (SFunCall args) = FunCallE (FunCall e args)
    foldSE e (SIndex e')     = IndexE e e'
    foldSE e (SSelect i)     = SelectE e i

    primaryExpToExpr (PIdent i) = IdentE i
    primaryExpToExpr (PParen e) = e

suffix :: Parser ExpSuffix
suffix = choice [ SFunCall <$> call, SIndex <$> index, SSelect <$> select ]
  where
    call   = between (token_ LParen) (token_ RParen) (expr `sepBy` token_ Comma)
    index  = between (token_ LBrack) (token_ RBrack) expr
    select = token_ Dot >> ident

suffixExp :: Parser SuffixExp
suffixExp = SuffixExp <$> primaryExp <*> many suffix

primaryExp :: Parser PrimaryExp
primaryExp = PIdent <$> ident <|> PParen <$> between (token_ LParen) (token_ RParen) expr

unop :: Parser Unop
unop = choice [ token Dash $> Neg
              , token Bang $> Not
              , token Ampers $> AddrOf
              , token Star $> Deref
              ]

binop :: Parser Binop
binop = choice [ token Plus $> Add
               , token Dash $> Sub
               , token Star $> Mul
               , token Slash $> Div
               , token Percent $> Rem
               ]

----------------------------------------------------------------------------------------------------

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
    testTok x = Left (S.singleton (Tokens (x :| [])), S.empty, S.empty)

stringLit :: Parser T.Text
stringLit = P.token testTok Nothing
  where
    testTok (L _ (StringLit str) _) = Right str
    testTok x = Left (S.singleton (Tokens (x:|[])), S.empty, S.empty)

numberLit :: Parser Number
numberLit = P.token testTok Nothing
  where
    testTok (L _ (Num n) _) = Right n
    testTok x = Left (S.singleton (Tokens (x :| [])), S.empty, S.empty)
