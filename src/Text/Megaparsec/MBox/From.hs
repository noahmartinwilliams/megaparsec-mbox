module Text.Megaparsec.MBox.From where

import Control.Monad
import Data.Either
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.EmailAddress
import Text.Megaparsec.MBox.DateTime
import Text.Megaparsec.MBox.EAddr
import Text.Megaparsec.MBox.Space
import Text.Megaparsec.MBox.Types

mboxFirstFrom :: MBoxParser (String, DateTime)
mboxFirstFrom = do
    void $ lexeme (string "From")
    nums <- some (digitChar)
    void $ single '@'
    chars <- lexeme (some alphaNumChar)
    dt <- hlexeme mboxFromDT
    return (nums ++ ('@' : chars), dt)

mboxFromName1 :: MBoxParser String
mboxFromName1 = some letterChar

mboxFromName2 :: MBoxParser String
mboxFromName2 = do
    void $ single '"'
    str <- some (noneOf "\"")
    void $ single '"'
    return (('"' : str) ++ "\"")

mboxFromName :: MBoxParser String
mboxFromName = (mboxFromName1 <|> mboxFromName2)

mboxSecondFrom :: MBoxParser (String, EAddr)
mboxSecondFrom = do 
    void $ string "From"
    void $ lexeme (single ':')
    name <- lexeme mboxFromName
    void $ single '<'
    addr <- mboxEAddr
    void $ single '>'
    return (name, addr)

