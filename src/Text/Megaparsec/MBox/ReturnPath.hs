module Text.Megaparsec.MBox.ReturnPath where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.EmailAddress
import Text.Megaparsec.MBox.EAddr
import Text.Megaparsec.MBox.Space as S
import Text.Megaparsec.MBox.Types

mboxReturnPath :: MBoxParser EAddr
mboxReturnPath = do
    void $ string "Return-Path"
    void $ S.lexeme (single ':')
    void $ single '<'
    addr <- mboxEAddr
    void $ S.lexeme (single '>')
    return addr
