import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Parser

main = defaultMain $ hUnitTestToTests $ TestList
  [ TestCase $ assertEqual "parse changelist without endline"
        (ChangelistSeparator "L1")
        (read "--- Changelist 'L1':")
  , TestCase $ assertEqual "parse changelist with endline"
        (ChangelistSeparator "L1")
        (read "--- Changelist 'L1':\n")
  , TestCase $ assertEqual "parse empty line"
        EmptyLine
        (read "")
  , TestCase $ assertEqual "parse endline"
        EmptyLine
        (read "")
  , TestCase $ assertEqual "parse file"
        (File $ SvnFile MsModified PsNoModification "trunk/icp_algorithm/cloud_io.hpp")
        (read "M       trunk/icp_algorithm/cloud_io.hpp")
  , TestCase $ assertEqual "real example"
        [ File $ SvnFile MsModified PsNoModification "trunk/icp_algorithm/cloud_io.hpp"
        ]
        (parseSvnOutput $ unlines [ "M       trunk/icp_algorithm/cloud_io.hpp"
                        ])

  , TestCase $ assertEqual "real example"
        [ File $ SvnFile MsModified PsNoModification "trunk/icp_algorithm/cloud_io.hpp"
        , File $ SvnFile MsUntracked PsNoModification "trunk/old_icp_algorithm/untracked_file"
        , EmptyLine
        , ChangelistSeparator "L1"
        , File $ SvnFile MsNoModification PsNoModification "readme.textile"
        ]
        (parseSvnOutput $ unlines [ "M       trunk/icp_algorithm/cloud_io.hpp"
                        , "?       trunk/old_icp_algorithm/untracked_file"
                        , ""
                        , "--- Changelist 'L1':"
                        , "        readme.textile"
                        ])
  ]