module Text.Megaparsec.MBox.Received where

import Control.Monad
import Data.Either
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.IP
import Text.Megaparsec.MBox.DateTime
import Text.Megaparsec.MBox.Space
import Text.Megaparsec.MBox.Types

mboxFirstReceived :: MBoxParser Received
mboxFirstReceived = do
    void $ string "Received"
    void $ hlexeme (single ':')
    void $ hlexeme (string "by")
    inp <- getInput
    off <- getOffset
    st <- getParserState
    let (st', addr) = runParser' ip st
    updateParserState (\_ -> st')
    if isLeft addr
    then
        let (Left e) = addr in fancyFailure (Set.fromList [(ErrorFail ("Invalid IP Address. " ++ (errorBundlePretty e)))])
    else do
        let (Right a) = addr
        void $ single ' '
        void $ hlexeme (string "with")
        void $ hlexeme (string "SMTP")
        void $ hlexeme (string "id")
        smtpID <- hlexeme (some (alphaNumChar))
        void $ lexeme (single ';')
        dateTime <- lexeme mboxReceivedDT
        return (Received a smtpID dateTime)
