module Main (main) where

----------------------------------------------------------------------------------------------------
import Data.Either (isRight)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit
----------------------------------------------------------------------------------------------------
import qualified Language.Nova.Lexer as L
----------------------------------------------------------------------------------------------------

testPgmRoot :: FilePath
testPgmRoot = "test_pgms"

main :: IO ()
main = do
    all_files <- getDirectoryContents testPgmRoot
    let nv_files = filter ((==) ".nv" . takeExtension) all_files
    defaultMain (testGroup "All tests" (all_tests nv_files))

all_tests :: [FilePath] -> [TestTree]
all_tests nv_files =
    [ testGroup "Lexer" (lexer_tests nv_files) ]

lexer_tests :: [FilePath] -> [TestTree]
lexer_tests nv_files =
    flip map nv_files $ \f -> testCase f $ do
      str <- readFile (testPgmRoot </> f)
      assertBool "lex result" (isRight (L.lex str))
