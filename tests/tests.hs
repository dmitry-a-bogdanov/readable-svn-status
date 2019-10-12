import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Parser
import Types
import Text.Parsec

parseLine = parse svnStatusLineParser ""

main = defaultMain $ hUnitTestToTests $ TestList
  [ TestCase $ assertEqual "parse changelist without endline"
        (Right $ ChangelistSeparator "L1")
        (parseLine "--- Changelist 'L1':\n")
  , TestCase $ assertEqual "parse empty line"
        (Right EmptyLine)
        (parseLine "\n")
  , TestCase $ assertEqual "parse file"
        (Right $ File $ (defaultFile "path/to/file") { modificationStatus = MsModified })
        (parseLine "M       path/to/file\n")
  , TestCase $ assertEqual "aFile"
        (Right $ File $ (defaultFile "path/to/file") { modificationStatus = MsModified })
        (parse svnFileParser "-" "M       path/to/file\n")
  , TestCase $ assertEqual "just one flag"
        (Right PsModified)
        (parse svnFileFlagParser "-" "M")
  ]