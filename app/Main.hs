module Main where

----------------------------------------------------------------------------------------------------
import qualified Language.Nova.Lexer as L
import qualified Language.Nova.Parser as P
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    f <- readFile "test_pgms/hello.nv"
    case L.lex f of
      Left err -> error (show err)
      Right ts -> do
        putStrLn "##### Tokens #####"
        mapM_ print ts
        putStrLn "##### AST ########"
        case P.runParser' P.parseModule ts of
          Left err -> error (show err)
          Right decls -> mapM_ print decls
