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

mboxArcVar :: MBoxParser (String, String)
mboxArcVar = do
    key <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (char '=')
    val <- S.lexeme (some (noneOf ";="))
    let val' = Prelude.filter (isNotSpace) val
    return (key, val)

isNotSpace :: Char -> Bool
isNotSpace '\n' = False
isNotSpace ' ' = False
isNotSpace '\t' = False
isNotSpace _ = True

mboxArcVars :: MBoxParser [(String, String)]
mboxArcVars = do
    arcVars <- S.lexeme (endBy1 (sepBy mboxArcVar (S.lexeme (single ';')) ) (string "=="))
    return (Prelude.foldr (++) [] arcVars)

