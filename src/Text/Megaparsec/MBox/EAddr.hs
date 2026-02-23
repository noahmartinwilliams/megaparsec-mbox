module Text.Megaparsec.MBox.EAddr where

import Data.Either
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.EmailAddress
import Text.Megaparsec.MBox.Types

mboxEAddr :: MBoxParser EAddr
mboxEAddr = do
    inp <- getInput
    off <- getOffset
    st <- getParserState
    let (st', addr) = runParser' eaddr st
    updateParserState (\_ -> st')
    if isLeft addr
    then
        let (Left e) = addr in fancyFailure (Set.fromList [(ErrorFail ("Invalid email Address. " ++ (errorBundlePretty e)))])
    else do
        let (Right a) = addr
        return a
