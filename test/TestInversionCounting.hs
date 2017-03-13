module TestInversionCounting where

import Test.HUnit
import InversionCounting

test_list1 = TestCase (assertEqual "inversionCount [1,3,5,2,4,6] 6"
  ([1,2,3,4,5,6],3) (inversionCount [1,3,5,2,4,6] 6))

test_list2 = TestCase (assertEqual "inversionCount [1,4,5,2,9,44,7,12,34] 9"
  ([1,2,4,5,7,9,12,34,44],6) (inversionCount [1,4,5,2,9,44,7,12,34] 9))

test_list3 = TestCase (assertEqual "inversionCount2 [1,3,5,2,4,6] 6"
  3 (inversionCount2 [1,3,5,2,4,6]))

test_list4 = TestCase (assertEqual "inversionCount2 [1,4,5,2,9,44,7,12,34] 9"
  6 (inversionCount2 [1,4,5,2,9,44,7,12,34]))


tests = TestList [TestLabel "test_list1" test_list1, TestLabel "test_list2" test_list2,
                  TestLabel "test_list3" test_list3,  TestLabel "test_list4" test_list4]

main = runTestTT tests
