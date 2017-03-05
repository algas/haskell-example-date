{-# LANGUAGE OverloadedStrings #-}
module WarekiInternalSpec where

import           Data.Time.Wareki.Internal
import           Test.Hspec

spec :: Spec
spec = do
    describe "fst3" $ do
        it "x" $
            fst3 ("1", "2", "3") `shouldBe` "1"
    describe "fromGregorian" $ do
        it "Nothing" $
            fromGregorian 1868 1 24 `shouldBe` Nothing
        it "Meiji" $ do
            fromGregorian 1868 1 25 `shouldBe` Just (Wareki Meiji 1 1 25)
            fromGregorian 1912 7 29 `shouldBe` Just (Wareki Meiji 45 7 29)
        it "Taisho" $ do
            fromGregorian 1912 7 30 `shouldBe` Just (Wareki Taisho 1 7 30)
            fromGregorian 1926 12 24 `shouldBe` Just (Wareki Taisho 15 12 24)
        it "Showa" $ do
            fromGregorian 1926 12 25 `shouldBe` Just (Wareki Showa 1 12 25)
            fromGregorian 1989 1 7 `shouldBe` Just (Wareki Showa 64 1 7)
        it "Heisei" $
            fromGregorian 1989 1 8 `shouldBe` Just (Wareki Heisei 1 1 8)
    describe "toGregorian" $ do
        it "Meiji" $ do
            toGregorian (Wareki Meiji 1 1 25) `shouldBe` (1868, 1, 25)
            toGregorian (Wareki Meiji 45 7 29) `shouldBe` (1912, 7, 29)
        it "Taisho" $ do
            toGregorian (Wareki Taisho 1 7 30) `shouldBe` (1912, 7, 30)
            toGregorian (Wareki Taisho 15 12 24) `shouldBe` (1926, 12, 24)
        it "Showa" $ do
            toGregorian (Wareki Showa 1 12 25) `shouldBe` (1926, 12, 25)
            toGregorian (Wareki Showa 64 1 7) `shouldBe` (1989, 1, 7)
        it "Heisei" $
            toGregorian (Wareki Heisei 1 1 8) `shouldBe` (1989, 1, 8)
    describe "toText" $ do
        it "Meiji" $ do
            toText (Wareki Meiji 1 1 25) `shouldBe` "明治1年1月25日"
            toText (Wareki Meiji 45 7 29) `shouldBe` "明治45年7月29日"
        it "Taisho" $ do
            toText (Wareki Taisho 1 7 30) `shouldBe` "大正1年7月30日"
            toText (Wareki Taisho 15 12 24) `shouldBe` "大正15年12月24日"
        it "Showa" $ do
            toText (Wareki Showa 1 12 25) `shouldBe` "昭和1年12月25日"
            toText (Wareki Showa 64 1 7) `shouldBe` "昭和64年1月7日"
        it "Heisei" $
            toText (Wareki Heisei 1 1 8) `shouldBe` "平成1年1月8日"
    describe "addDays" $ do
        it "Meiji" $ do
            addDays (-1) (Wareki Meiji 1 1 25) `shouldBe` Nothing
            addDays 1 (Wareki Meiji 1 1 25) `shouldBe` Just (Wareki Meiji 1 1 26)
            addDays 1 (Wareki Meiji 45 7 29) `shouldBe` Just (Wareki Taisho 1 7 30)
        it "Taisho" $ do
            addDays (-1) (Wareki Taisho 1 7 30) `shouldBe` Just (Wareki Meiji 45 7 29)
            addDays 2 (Wareki Taisho 1 7 30) `shouldBe` Just (Wareki Taisho 1 8 1)
            addDays 1 (Wareki Taisho 15 12 24) `shouldBe` Just (Wareki Showa 1 12 25)
        it "Showa" $ do
            addDays (-1) (Wareki Showa 1 12 25) `shouldBe` Just (Wareki Taisho 15 12 24)
            addDays 7 (Wareki Showa 1 12 25) `shouldBe` Just (Wareki Showa 2 1 1)
            addDays 1 (Wareki Showa 64 1 7) `shouldBe` Just (Wareki Heisei 1 1 8)
        it "Heisei" $ do
            addDays (-1) (Wareki Heisei 1 1 8) `shouldBe` Just (Wareki Showa 64 1 7)
            addDays 1 (Wareki Heisei 1 1 8) `shouldBe` Just (Wareki Heisei 1 1 9)
    describe "diffDays" $ do
        it "Meiji" $ do
            diffDays (Wareki Meiji 1 1 26) (Wareki Meiji 1 1 25) `shouldBe` 1
            diffDays (Wareki Taisho 1 7 30) (Wareki Meiji 45 7 29) `shouldBe` 1
        it "Taisho" $ do
            diffDays (Wareki Taisho 1 8 1) (Wareki Taisho 1 7 30) `shouldBe` 2
            diffDays (Wareki Showa 1 12 25) (Wareki Taisho 15 12 24) `shouldBe` 1
        it "Showa" $ do
            diffDays (Wareki Showa 2 1 1) (Wareki Showa 1 12 25) `shouldBe` 7
            diffDays (Wareki Heisei 1 1 8) (Wareki Showa 64 1 7) `shouldBe` 1
        it "Heisei" $ do
            diffDays (Wareki Heisei 1 1 9) (Wareki Heisei 1 1 8) `shouldBe` 1
