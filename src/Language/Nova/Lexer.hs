module Language.Nova.Lexer where

----------------------------------------------------------------------------------------------------
import Data.Functor
import qualified Data.Text as T
import Prelude hiding (lex)
import Text.Megaparsec as P
import Text.Megaparsec.String as P
----------------------------------------------------------------------------------------------------

data Tok
  = Add
  | Ampers
  | And
  | Assign
  | BOr
  | Colon
  | Comma
  | Div
  | Dot
  | Equal
  | Id T.Text
  | LAngle
  | LBrace
  | LBrack
  | LParen
  | NotEqual
  | Num Number
  | Or
  | RAngle
  | RBrace
  | RBrack
  | Rem
  | RParen
  | ShiftL
  | ShiftR
  | Star
  | StringLit T.Text
  | Sub
  | XOr
  deriving (Show)

data Number = Number T.Text (Maybe NumType)
  deriving (Show)

data NumType
  -- Signed
  = I8 | I16 | I32 | I64
  -- Unsigned
  | U8 | U16 | U32 | U64
  -- Float
  | F32 | F64
  deriving (Show)

lex :: String -> Either (ParseError Char Dec) [Tok]
lex = parse (skipTrailing >> many lexOne <* eof) ""

-- | Lex a single token. Consumes trailing whitespace and comments.
lexOne :: Parser Tok
lexOne =
    choice
      [ StringLit <$> lexString
      , Num <$> lexNumber
      , string "&&" $> And
      , string "||" $> Or
      , string ">>" $> ShiftR
      , string "<<" $> ShiftL
      , string "==" $> Equal
      , string "/=" $> NotEqual
      , char ':' $> Colon
      , char '(' $> LParen
      , char ')' $> RParen
      , char '[' $> LBrack
      , char ']' $> RBrack
      , char '{' $> LBrace
      , char '}' $> RBrace
      , char '<' $> LAngle
      , char '>' $> RAngle
      , char '&' $> Ampers
      , char '|' $> BOr
      , char '=' $> Assign
      , char '.' $> Dot
      , char ',' $> Comma
      , char '*' $> Star
      , char '/' $> Div
      , char '%' $> Rem
      , char '+' $> Add
      , char '-' $> Sub
      , char '^' $> XOr
      , Id <$> lexIdent
      ]
    <* skipTrailing

-- | Skip comments and whitespace.
skipTrailing :: Parser ()
skipTrailing =
    skipMany (void (some spaceChar) <|> void (char '#' >> many (satisfy (/= '\n')) >> optional (char '\n')))

lexString :: Parser T.Text
lexString =
    T.pack <$>
    between (char '"') (char '"')
            (many (noneOf ['"', '\\'] <|> (char '\\' >> escapeSeq)))

lexNumber :: Parser Number
lexNumber = do
    ds1 <- some digitChar
    ds2 <-
      optional (char '.') >>= \case
        Just _  -> Just <$> some digitChar
        Nothing -> return Nothing
    ty  <- optional lexNumType
    case (ds2, ty) of
      (Just ds2', Just F32) -> return $ Number (T.pack (ds1 ++ "." ++ ds2')) ty
      (Just ds2', Just F64) -> return $ Number (T.pack (ds1 ++ "." ++ ds2')) ty
      (Just ds2', Nothing ) -> return $ Number (T.pack (ds1 ++ "." ++ ds2')) ty
      (Just _   , _       ) -> fail ("Floating point with unexpected type: " ++ show ty)
      (Nothing  , Just F32) -> fail ("Integral type with floating point type: " ++ show ty)
      (Nothing  , Just F64) -> fail ("Integral type with floating point type: " ++ show ty)
      (Nothing  , _       ) -> return $ Number (T.pack ds1) ty

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

lexIdent :: Parser T.Text
lexIdent = do
    c1 <- firstChar
    cs <- many idChar
    return (T.pack (c1 : cs))
  where
    firstChar = oneOf ['a' .. 'z'] <|> oneOf ['A' .. 'Z'] <|> char '_'
    idChar = firstChar <|> digitChar

escapeSeq :: Parser Char
escapeSeq = choice
    [ char 'n' $> '\n' ]
