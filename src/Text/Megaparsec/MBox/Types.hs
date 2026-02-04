module Text.Megaparsec.MBox.Types where

import Text.Megaparsec
import Data.Void

type MBoxParser = Parsec Void String 

data DOW = Sun | Mon | Tues | Wed | Thur | Fri | Sat deriving(Show, Eq)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving(Show, Eq)

data DateTime = DateTime { dtYear :: Int, dtMonth :: Month, dtDOW :: DOW, dtHour :: Int, dtMinute :: Int, dtSecond :: Int, dtOffset :: Int } deriving(Show, Eq)
