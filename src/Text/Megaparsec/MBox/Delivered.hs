module Text.Megaparsec.MBox.Delivered where

import Control.Monad
import Data.Either
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.EmailAddress
import Text.Megaparsec.MBox.Space
import Text.Megaparsec.MBox.Types

mboxDeliveredTo :: MBoxParser EAddr
mboxDeliveredTo = do
    void $ hlexeme (string "Delivered-To:")
    inp <- getInput
    off <- getOffset
    st <- getParserState
    let (st', addr) = runParser' eaddr st
    updateParserState (\_ -> st')
    if isLeft addr
    then
        let (Left e) = addr in fancyFailure (Set.fromList [(ErrorFail ("Invalid email address. " ++ (errorBundlePretty e)))])
    else do
        let (Right a) = addr
        void $ eol
        return a
