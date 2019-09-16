import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Parser

main = defaultMain $ hUnitTestToTests $ TestList
  [ TestCase $ assertEqual "parse changelist without endline"
        (ChangelistSeparator "L1")
        (parseLine "--- Changelist 'L1':")
  , TestCase $ assertEqual "parse empty line"
        EmptyLine
        (parseLine "")
  , TestCase $ assertEqual "parse endline"
        EmptyLine
        (parseLine "")
  , TestCase $ assertEqual "parse file"
        (File $ SvnFile MsModified PsNoModification "trunk/icp_algorithm/cloud_io.hpp")
        (parseLine "M       trunk/icp_algorithm/cloud_io.hpp")
  ]