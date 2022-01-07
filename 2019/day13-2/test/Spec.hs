-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

module Main where

import Intcode
import Test.Hspec

example1 = "3,9,8,9,10,9,4,9,99,-1,8"
example2 = "3,9,7,9,10,9,4,9,99,-1,8"
example3 = "3,3,1108,-1,8,3,4,3,99"
example4 = "3,3,1107,-1,8,3,4,3,99"
example5 = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
example6 = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
example7 = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
example8 = "101,-1,7,7,4,7,1105,11,0,99"
example9 = "1,0,3,3,1005,2,10,5,1,0,4,1,99"
example10 = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
example11 = "1102,34915192,34915192,7,4,7,99,0"
example12 = "104,1125899906842624,99"

main = hspec $ do
  describe "example 1" $ do
    it "outputs 1 if input is 8" $
      parseAndExecute example1 [8] `shouldBe` [1]
    it "outputs 0 if input is not 8" $
      parseAndExecute example1 [7] `shouldBe` [0]
    it "outputs 0 if input is not 8" $
      parseAndExecute example1 [9] `shouldBe` [0]
  describe "example 2" $ do
    it "outputs 1 if input is <= 8" $
      parseAndExecute example2 [7] `shouldBe` [1]
    it "outputs 1 if input is < 8" $
      parseAndExecute example2 [-7] `shouldBe` [1]
    it "outputs 0 if input is >= 8" $
      parseAndExecute example2 [8] `shouldBe` [0]
    it "outputs 0 if input is > 8" $
      parseAndExecute example2 [9] `shouldBe` [0]
  describe "example 3" $ do
    it "outputs 1 if input is 8" $
      parseAndExecute example3 [8] `shouldBe` [1]
    it "outputs 0 if input is not 8" $
      parseAndExecute example3 [7] `shouldBe` [0]
    it "outputs 0 if input is not 8" $
      parseAndExecute example3 [9] `shouldBe` [0]
  describe "example 4" $ do
    it "outputs 1 if input is < 8" $
      parseAndExecute example4 [7] `shouldBe` [1]
    it "outputs 1 if input is < 8" $
      parseAndExecute example4 [-7] `shouldBe` [1]
    it "outputs 0 if input is >= 8" $
      parseAndExecute example4 [8] `shouldBe` [0]
    it "outputs 0 if input is >= 8" $
      parseAndExecute example4 [9] `shouldBe` [0]
  describe "example 5" $ do
    it "outputs 0 if input is 0" $
      parseAndExecute example5 [0] `shouldBe` [0]
    it "outputs 1 if input is not 0" $
      parseAndExecute example5 [1] `shouldBe` [1]
    it "outputs 0 if input is not 8" $
      parseAndExecute example5 [-42] `shouldBe` [1]
  describe "example 6" $ do
    it "outputs 0 if input is 0" $
      parseAndExecute example6 [0] `shouldBe` [0]
    it "outputs 1 if input is not 0" $
      parseAndExecute example6 [1] `shouldBe` [1]
    it "outputs 0 if input is not 8" $
      parseAndExecute example6 [-42] `shouldBe` [1]
  describe "example 7" $ do
    it "outputs 999 if input is < 8" $
      parseAndExecute example7 [-10] `shouldBe` [999]
    it "outputs 999 if input is < 8" $
      parseAndExecute example7 [7] `shouldBe` [999]
    it "outputs 1000 if input is 8" $
      parseAndExecute example7 [8] `shouldBe` [1000]
    it "outputs 1001 if input is > 8" $
      parseAndExecute example7 [9] `shouldBe` [1001]
    it "outputs 1001 if input is > 8" $
      parseAndExecute example7 [42] `shouldBe` [1001]
  describe "example 8" $
    it "should output 10,9..0" $
      parseAndExecute example8 [] `shouldBe` [10,9..0]
  describe "example 9" $
    it "should output 0" $
      parseAndExecute example9 [] `shouldBe` [0]
  describe "example 10" $
    it "should return a copy of itself" $
      parseAndExecute example10 [] `shouldBe` [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  describe "example 11" $
    it "should output a 16 digits number" $
      parseAndExecute example11 [] `shouldSatisfy` ((==16) . length . show . head)
  describe "example 12" $
    it "should output the number in the middle" $
      parseAndExecute example12 [] `shouldBe` [1125899906842624]