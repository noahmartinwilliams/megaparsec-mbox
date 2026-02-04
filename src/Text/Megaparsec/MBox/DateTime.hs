module Text.Megaparsec.MBox.DateTime where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.MBox.Space as S
import Text.Megaparsec.MBox.Types

mboxDOW :: MBoxParser DOW 
mboxDOW = do
    dowStr <- (string "Sun" <|> string "Mon" <|> string "Tues" <|> string "Wed" <|> string "Thur" <|> string "Fri" <|> string "Sat")
    case dowStr of
        "Sun" -> return Sun
        "Mon" -> return Mon
        "Tues" -> return Tues
        "Wed" -> return Wed
        "Thur" -> return Thur
        "Fri" -> return Fri
        "Sat" -> return Sat

mboxMonth :: MBoxParser Month
mboxMonth = do
    mon <- ( string "Jan" <|> string "Feb" <|> string "Mar" <|> string "Apr" <|> string "May" <|> string "Jun" <|> string "Jul" <|> string "Aug" <|> string "Sep" <|> string "Oct" <|> string "Nov" <|> string "Dec" )

    case mon of
        "Jan" -> return Jan
        "Feb" -> return Feb
        "Mar" -> return Mar
        "Apr" -> return Apr
        "May" -> return May
        "Jun" -> return Jun
        "Jul" -> return Jul
        "Aug" -> return Aug
        "Sep" -> return Sep
        "Oct" -> return Oct
        "Nov" -> return Nov
        "Dec" -> return Dec

mboxDD :: MBoxParser Int
mboxDD = do
    i1 <- digitChar
    i2 <- digitChar
    let i1i = read [i1] :: Int
        i2i = read [i2] :: Int
    return (i1i * 10 + i2i)

mboxDOM :: MBoxParser Int
mboxDOM = mboxDD

mboxTime :: MBoxParser (Int, Int, Int)
mboxTime = do
    hours <- mboxDD
    void $ single ':'
    minutes <- mboxDD
    void $ single ':'
    seconds <- mboxDD
    return (hours, minutes, seconds)

mboxTimeOffset :: MBoxParser Int
mboxTimeOffset = do
    s <- (single '+' <|> single '-')
    t <- Text.Megaparsec.count 4 digitChar
    if s == '+'
    then
        return (read t :: Int)
    else
        return (- (read t :: Int))

mboxYear :: MBoxParser Int
mboxYear = do
    y <- Text.Megaparsec.count 4 digitChar -- update this to five digits in 7974 years
    return (read y :: Int)

mboxFromDT :: MBoxParser DateTime
mboxFromDT = do
    dow <- S.lexeme mboxDOW
    mon <- S.lexeme mboxMonth
    dom <- S.lexeme mboxDOM
    (hour, minute, second) <- S.lexeme mboxTime
    off <- S.lexeme mboxTimeOffset
    year <- S.lexeme mboxYear
    return (DateTime { dtYear = year, dtMonth = mon, dtDOW=dow, dtHour = hour, dtMinute = minute, dtSecond = second, dtOffset = off})
