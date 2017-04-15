module Main where

----------------------------------------------------------------------------------------------------
import Text.PrettyPrint
import System.FilePath
import System.Environment (getArgs)
----------------------------------------------------------------------------------------------------
import qualified Language.Nova.CodeGen as CG
import qualified Language.Nova.Lexer as L
import qualified Language.Nova.Parser as P
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    [file] <- getArgs
    f <- readFile file
    case L.lex f of
      Left err -> error (show err)
      Right ts -> do
        putStrLn "##### Tokens #####"
        mapM_ print ts
        putStrLn "##### AST ########"
        case P.runParser' P.parseModule ts of
          Left err -> error (show err)
          Right decls -> do
            mapM_ print decls
            writeFile (replaceExtension file "c") (render (CG.codegen decls))
