module Main where

----------------------------------------------------------------------------------------------------
import qualified Language.Nova.Lexer as L
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    f <- readFile "test_pgms/hello.nv"
    case L.lex f of
      Left err -> print err
      Right ts -> mapM_ print ts
