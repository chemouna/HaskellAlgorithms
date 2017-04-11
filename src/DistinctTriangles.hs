
module DistinctTriangles where

{-
Problem definition: https://www.codeeval.com/browse/188/ 
-}

import Test.Hspec

distinctTriangles :: String -> Int
distinctTriangles = undefined

countTriangles :: [[Int]] -> Int
countTriangles = undefined


test = hspec $ do
  describe "Distinct Trianges - " $ do 
    it "countTriangles s0" $ do   countTriangles [[0, 2], [0, 1], [1, 2], [1, 3], [2, 3]] `shouldBe` 2
    it "countTriangles s1" $ do   countTriangles [[1, 3], [1, 8], [3, 8]] `shouldBe` 1
    it "countTriangles s2" $ do   countTriangles [[5, 6], [5, 7], [6, 7]] `shouldBe` 1

    it "distinctTriangles s0" $ do   distinctTriangles "4 5;0 2,0 1,1 2,1 3,2 3" `shouldBe` 2
    it "distinctTriangles s1" $ do   distinctTriangles "9 3;1 3,1 8,3 8" `shouldBe` 1
    it "distinctTriangles s2" $ do   distinctTriangles "9 3;5 6,5 7,6 7" `shouldBe` 1
    it "distinctTriangles s2" $ do   distinctTriangles "9 3;5 6,5 7,5 7,6 7,7 5" `shouldBe` 1

    it "distinctTriangles s3" $ do   distinctTriangles "9 3;3 7,3 7,1 1" `shouldBe` 0
    it "distinctTriangles s4" $ do   distinctTriangles "9 3;1 3,1 8,3 8" `shouldBe` 1
    it "distinctTriangles s5" $ do   distinctTriangles "9 6;3 4,1 4,1 3,1 8,3 8,4 8" `shouldBe` 4
    it "distinctTriangles s6" $ do   distinctTriangles "12 14;0 1,0 4,1 2,1 4,2 3,2 7,3 7,4 8,4 9,7 10,7 11,8 9,9 10,10 11" `shouldBe` 4
    it "distinctTriangles s7" $ do   distinctTriangles "4 5;0 2,0 1,1 2,1 3,2 3" `shouldBe` 2
    it "distinctTriangles s8" $ do   distinctTriangles "9 5;1 3,1 4,1 8,3 8,4 8" `shouldBe` 2
    it "distinctTriangles s9" $ do   distinctTriangles "9 4;1 3,1 5,3 4,4 5" `shouldBe` 0
    it "distinctTriangles s10" $ do   distinctTriangles "9 13;0 1,0 3,1 2,1 3,1 4,1 5,2 5,3 4,3 6,5 7,5 8,6 7,7 8" `shouldBe` 4
    it "distinctTriangles s11" $ do   distinctTriangles "9 3;5 6,5 7,6 7" `shouldBe` 1
    it "distinctTriangles s12" $ do   distinctTriangles "9 5;1 3,1 5,3 4,4 5,1 4" `shouldBe` 2

