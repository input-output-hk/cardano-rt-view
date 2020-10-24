import Test.Tasty
import Test.Tasty.HUnit

import Test.Cardano.RTView.HtmlCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "2+2=4" $
      2+2 @?= 4
  -- , testCase "7 is even" $
  --    assertBool "Oops, 7 is odd" (even 7)
  ]
