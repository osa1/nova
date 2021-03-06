{-# LANGUAGE DeriveFunctor #-}

module Language.Nova.Lexer where

----------------------------------------------------------------------------------------------------
import Data.Functor
import qualified Data.Text as T
import Prelude hiding (lex)
import Data.List (intercalate)
import Text.Megaparsec as P
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec.String as P
----------------------------------------------------------------------------------------------------

newtype Ident = Ident { identText :: T.Text }
  deriving (Show, Eq, Ord)

data Tok
  = Ampers
  | And
  | Assign
  | Bang
  | BOr
  | Colon
  | Comma
  | Dash
  | Dot
  | Else
  | ElseIf
  | Equal
  | Fn
  | Id Ident
  | If
  | Include
  | LAngle
  | LBrace
  | LBrack
  | LParen
  | NotEqual
  | Num Number
  | Or
  | Percent
  | Plus
  | RAngle
  | RBrace
  | RBrack
  | Return
  | RParen
  | ShiftL
  | ShiftR
  | Slash
  | Star
  | StringLit T.Text
  | Var
  | While
  | XOr
  deriving (Show, Eq, Ord)

data Loc a = L SourcePos a SourcePos
  deriving (Show, Functor, Eq, Ord)

instance Show a => ShowToken (Loc a) where
  showTokens ts = intercalate "," (map showL (NE.toList ts))
    where
      showL (L (SourcePos _ l c) p _) = show p ++ "(" ++ show (unPos l) ++ "," ++ show (unPos c) ++ ")"

unLoc :: Loc a -> a
unLoc (L _ a _) = a

data Number = Number (Either Integer Double) (Maybe NumType)
  deriving (Show, Eq, Ord)

data NumType
  -- Signed
  = I8 | I16 | I32 | I64
  -- Unsigned
  | U8 | U16 | U32 | U64
  -- Float
  | F32 | F64
  deriving (Show, Eq, Ord)

lex :: String -> Either (ParseError Char Dec) [Loc Tok]
lex = parse (skipTrailing >> many lexOne <* eof) ""

-- | Lex a single token. Consumes trailing whitespace and comments.
lexOne :: Parser (Loc Tok)
lexOne =
    choice
      [ fmap StringLit <$> lexString
      , fmap Num <$> lexNumber
      , stringL "return" Return
      , stringL "if" If
      , stringL "elseif" ElseIf
      , stringL "else" Else
      , stringL "var" Var
      , stringL "while" While
      , stringL "&&" And
      , stringL "||" Or
      -- , stringL ">>" ShiftR -- confuses template param parser
      , stringL "<<" ShiftL
      , stringL "==" Equal
      , stringL "/=" NotEqual
      , stringL "fn" Fn
      , charL ':' Colon
      , charL '(' LParen
      , charL ')' RParen
      , charL '[' LBrack
      , charL ']' RBrack
      , charL '{' LBrace
      , charL '}' RBrace
      , charL '<' LAngle
      , charL '>' RAngle
      , charL '&' Ampers
      , charL '|' BOr
      , charL '=' Assign
      , charL '.' Dot
      , charL ',' Comma
      , charL '*' Star
      , charL '/' Slash
      , charL '%' Percent
      , charL '+' Plus
      , charL '-' Dash
      , charL '^' XOr
      , charL '!' Bang
      , stringL "include" Include
      , fmap Id <$> lexIdent
      ]
    <* skipTrailing

charL :: Char -> Tok -> Parser (Loc Tok)
charL c t = do
    p1 <- getPosition
    void (char c)
    p2 <- getPosition
    return (L p1 t p2)

stringL :: String -> Tok -> Parser (Loc Tok)
stringL s t = do
    p1 <- getPosition
    void (string s)
    p2 <- getPosition
    return (L p1 t p2)

-- | Skip comments and whitespace.
skipTrailing :: Parser ()
skipTrailing =
    skipMany (void (some spaceChar) <|> void (char '#' >> many (satisfy (/= '\n')) >> optional (char '\n')))

lexString :: Parser (Loc T.Text)
lexString = do
    p1  <- getPosition
    str <- between (char '"') (char '"')
                   (many (noneOf ['"', '\\'] <|> (char '\\' >> escapeSeq)))
    p2 <- getPosition
    return (L p1 (T.pack str) p2)

lexNumber :: Parser (Loc Number)
lexNumber = do
    p1  <- getPosition
    ds1 <- some digitChar
    ds2 <-
      optional (char '.') >>= \case
        Just _  -> Just <$> some digitChar
        Nothing -> return Nothing
    ty  <- optional lexNumType
    p2  <- getPosition
    num <- case (ds2, ty) of
      (Just ds2', Just F32) -> return $ Number (Right (read (ds1 ++ "." ++ ds2'))) ty
      (Just ds2', Just F64) -> return $ Number (Right (read (ds1 ++ "." ++ ds2'))) ty
      (Just ds2', Nothing ) -> return $ Number (Right (read (ds1 ++ "." ++ ds2'))) ty
      (Just _   , _       ) -> fail ("Floating point with unexpected type: " ++ show ty)
      (Nothing  , Just F32) -> fail ("Integral type with floating point type: " ++ show ty)
      (Nothing  , Just F64) -> fail ("Integral type with floating point type: " ++ show ty)
      (Nothing  , _       ) -> return $ Number (Left (read ds1)) ty
    return (L p1 num p2)

lexNumType :: Parser NumType
lexNumType = choice
    [ string "i8"  $> I8
    , string "i16" $> I16
    , string "i32" $> I32
    , string "i64" $> I64
    , string "u8"  $> U8
    , string "u16" $> U16
    , string "u32" $> U32
    , string "u64" $> U64
    , string "f32" $> F32
    , string "f64" $> F64
    ]

lexIdent :: Parser (Loc Ident)
lexIdent = do
    p1 <- getPosition
    c1 <- firstChar
    cs <- many idChar
    p2 <- getPosition
    return (L p1 (Ident (T.pack (c1 : cs))) p2)
  where
    firstChar = oneOf ['a' .. 'z'] <|> oneOf ['A' .. 'Z'] <|> char '_'
    idChar = firstChar <|> digitChar

escapeSeq :: Parser Char
escapeSeq = choice
    [ char 'n' $> '\n' ]
