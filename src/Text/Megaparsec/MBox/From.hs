module Text.Megaparsec.MBox.From where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.MBox.DateTime
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
