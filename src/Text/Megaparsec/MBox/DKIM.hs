module Text.Megaparsec.MBox.DKIM where

import Control.Monad
import Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.MBox.Arc as A
import Text.Megaparsec.MBox.Space as S
import Text.Megaparsec.MBox.Types

mboxDKIMSignature :: MBoxParser (Map String String)
mboxDKIMSignature = do
    void $ S.lexeme (string "DKIM-Signature")
    void $ S.lexeme (single ':')
    v <- S.lexeme mboxDKIMVars
    return (M.fromList v)

mboxDKIMVar :: MBoxParser (String, String)
mboxDKIMVar = do
    name <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (single '=')
    val <- S.lexeme (some (noneOf ";="))
    let val' = Prelude.filter (isNotSpace) val
    return (name, val')

mboxDKIMVars :: MBoxParser [(String, String)]
mboxDKIMVars = do
    v <- S.lexeme (endBy1 (sepBy mboxDKIMVar (S.lexeme (string ";" <|> string "=;"))) (single '='))
    return (Prelude.foldr (++) [] v)
