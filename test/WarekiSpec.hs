{-# LANGUAGE OverloadedStrings #-}
module WarekiSpec where

import           Data.Maybe
import           Data.Time.Wareki
import           Test.Hspec

spec :: Spec
spec = do
    describe "fromGregorian" $ do
        it "Nothing" $
            show (fromGregorian 1868 1 24) `shouldBe` "Nothing"
        it "Meiji" $ do
            show (fromGregorian 1868 1 25) `shouldBe` "Just (Wareki Meiji 1 1 25)"
            show (fromGregorian 1912 7 29) `shouldBe` "Just (Wareki Meiji 45 7 29)"
        it "Taisho" $ do
            show (fromGregorian 1912 7 30) `shouldBe` "Just (Wareki Taisho 1 7 30)"
            show (fromGregorian 1926 12 24) `shouldBe` "Just (Wareki Taisho 15 12 24)"
        it "Showa" $ do
            show (fromGregorian 1926 12 25) `shouldBe` "Just (Wareki Showa 1 12 25)"
            show (fromGregorian 1989 1 7) `shouldBe` "Just (Wareki Showa 64 1 7)"
        it "Heisei" $
            show (fromGregorian 1989 1 8) `shouldBe` "Just (Wareki Heisei 1 1 8)"

    describe "toGregorian" $ do
        it "Meiji" $ do
            (toGregorian . fromJust) (fromGregorian 1868 1 25) `shouldBe` (1868, 1, 25)
            (toGregorian . fromJust) (fromGregorian 1912 7 29) `shouldBe` (1912, 7, 29)
        it "Taisho" $ do
            (toGregorian . fromJust) (fromGregorian 1912 7 30) `shouldBe` (1912, 7, 30)
            (toGregorian . fromJust) (fromGregorian 1926 12 24) `shouldBe` (1926, 12, 24)
        it "Showa" $ do
            (toGregorian . fromJust) (fromGregorian 1926 12 25) `shouldBe` (1926, 12, 25)
            (toGregorian . fromJust) (fromGregorian 1989 1 7) `shouldBe` (1989, 1, 7)
        it "Heisei" $
            (toGregorian . fromJust) (fromGregorian 1989 1 8) `shouldBe` (1989, 1, 8)

    describe "toText" $ do
        it "Meiji" $ do
            (toText . fromJust) (fromGregorian 1868 1 25) `shouldBe` "明治1年1月25日"
            (toText . fromJust) (fromGregorian 1912 7 29) `shouldBe` "明治45年7月29日"
        it "Taisho" $ do
            (toText . fromJust) (fromGregorian 1912 7 30) `shouldBe` "大正1年7月30日"
            (toText . fromJust) (fromGregorian 1926 12 24) `shouldBe` "大正15年12月24日"
        it "Showa" $ do
            (toText . fromJust) (fromGregorian 1926 12 25) `shouldBe` "昭和1年12月25日"
            (toText . fromJust) (fromGregorian 1989 1 7) `shouldBe` "昭和64年1月7日"
        it "Heisei" $
            (toText . fromJust) (fromGregorian 1989 1 8) `shouldBe` "平成1年1月8日"
