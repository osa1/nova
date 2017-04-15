module Main (main) where

----------------------------------------------------------------------------------------------------
import Data.Either (isRight)
import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit
----------------------------------------------------------------------------------------------------
import qualified Language.Nova.Parser as P
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
    [ testGroup "Parser" (parser_tests nv_files) ]

parser_tests :: [FilePath] -> [TestTree]
parser_tests nv_files =
    flip map nv_files $ \f -> testCase f $ do
      str <- readFile (testPgmRoot </> f)
      assertBool "parse result" (isRight (P.runParser P.parseModule str))
