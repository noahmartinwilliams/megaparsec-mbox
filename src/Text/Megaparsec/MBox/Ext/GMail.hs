module Text.Megaparsec.MBox.Ext.GMail where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.MBox.Ext.GMail.Types
import Text.Megaparsec.MBox.Space as S
import Text.Megaparsec.MBox.Types

mboxGmailCategory :: MBoxParser String
mboxGmailCategory = do
    str <- some (alphaNumChar <|> (single ' '))
    return str

mboxGmailLabels :: MBoxParser GMailLabels 
mboxGmailLabels = do
    void $ S.lexeme (string "X-Gmail-Labels")
    void $ S.lexeme (single ':')
    strs <- S.lexeme (sepBy mboxGmailCategory (single ','))
    return strs

