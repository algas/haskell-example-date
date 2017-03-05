{-# LANGUAGE OverloadedStrings #-}
module Data.Time.Wareki.Internal where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as C

data Gengoh = Meiji | Taisho | Showa | Heisei
    deriving (Show, Ord, Eq)

data Wareki = Wareki Gengoh Integer Int Int
    deriving (Show, Ord, Eq)

meiji1 = C.fromGregorian 1868 1 25
taisho1 = C.fromGregorian 1912 7 30
showa1 = C.fromGregorian 1926 12 25
heisei1 = C.fromGregorian 1989 1 8

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

curry3 f x y z = f (x,y,z)

uncurry3 f (x,y,z) = f x y z

toGregorian' :: Day -> Integer -> Int -> Int -> (Integer, Int, Int)
toGregorian' day y m d = (y + fst3 (C.toGregorian day) - 1, m, d)

toGregorian :: Wareki -> (Integer, Int, Int)
toGregorian (Wareki Meiji y m d)  = toGregorian' meiji1 y m d
toGregorian (Wareki Taisho y m d) = toGregorian' taisho1 y m d
toGregorian (Wareki Showa y m d)  = toGregorian' showa1 y m d
toGregorian (Wareki Heisei y m d) = toGregorian' heisei1 y m d

fromGregorian :: Integer -> Int -> Int -> Maybe Wareki
fromGregorian y m d = fromDay $ C.fromGregorian y m d

toDay :: Wareki -> Day
toDay = uncurry3 C.fromGregorian . toGregorian

fromDay' :: Day -> Day -> (Integer, Int, Int)
fromDay' day1 day2 = (y1-y2+1, m, d)
    where
        (y1,m,d) = C.toGregorian day1
        y2 = fst3 $ C.toGregorian day2

fromDay :: Day -> Maybe Wareki
fromDay day
    | C.diffDays heisei1 day <= 0 = f Heisei heisei1
    | C.diffDays showa1 day <= 0 = f Showa showa1
    | C.diffDays taisho1 day <= 0 = f Taisho taisho1
    | C.diffDays meiji1 day <= 0 = f Meiji meiji1
    | otherwise = Nothing
    where
        f g d = Just $ uncurry3 (Wareki g) (fromDay' day d)

_toText :: Show a => a -> Text
_toText = T.pack . show

_textGengoh :: Gengoh -> Text
_textGengoh Meiji = "明治"
_textGengoh Taisho = "大正"
_textGengoh Showa = "昭和"
_textGengoh Heisei = "平成"

toText :: Wareki -> Text
toText (Wareki g y m d) = T.concat [_textGengoh g, _toText y, "年", _toText m, "月", _toText d, "日"]

addDays :: Integer -> Wareki -> Maybe Wareki
addDays days = fromDay . C.addDays days . toDay

diffDays :: Wareki -> Wareki -> Integer
diffDays x y = C.diffDays (toDay x) (toDay y)
