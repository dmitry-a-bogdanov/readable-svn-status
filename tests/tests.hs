import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main = defaultMain $ hUnitTestToTests $ TestList
  [ TestCase $ assertEqual "Foo == Foo" "Foo" "Foo"
  , TestCase $ assertEqual "Foo != Bar" "Foo" "Bar"
  ]