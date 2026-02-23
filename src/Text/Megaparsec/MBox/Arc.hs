module Text.Megaparsec.MBox.Arc(mboxArcSeal) where


import Control.Monad
import Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.MBox.Types
import Text.Megaparsec.MBox.Space as S

mboxArcSeal :: MBoxParser (M.Map String String)
mboxArcSeal = do
    void $ (string "ARC-Seal")
    void $ S.lexeme (single ':')
    vars <- S.lexeme mboxArcVars
    return (M.fromList vars)

mboxArcMessageSignature :: MBoxParser (M.Map String String, String)
mboxArcMessageSignature = do
    void $ S.lexeme (string "ARC-Message-Signature:")
    vars <- S.lexeme mboxArcVars
    void $ S.lexeme (single ';')
    void $ string "dara"
    void $ single '='
    dara <- some (anySingleBut '\n')
    void $ eol
    return (M.fromList vars, dara)


mboxArcVar :: MBoxParser (String, String)
mboxArcVar = do
    key <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (char '=')
    val <- S.lexeme (some (noneOf ";="))
    void $ optional (string "=")
    let val' = Prelude.filter (isNotSpace) val
    return (key, val')

mboxArcVars :: MBoxParser [(String, String)]
mboxArcVars = do
    arcVars <- S.lexeme (endBy1 (sepBy mboxArcVar (S.lexeme (single ';')) ) ((string "=") <|> (string "==")))
    return (Prelude.foldr (++) [] arcVars)


