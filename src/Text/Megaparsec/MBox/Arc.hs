module Text.Megaparsec.MBox.Arc(mboxArcSeal) where


import Control.Monad
import Data.Map as M
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.MBox.Types
import Text.Megaparsec.MBox.Space as S

mboxArcSeal :: Parser (M.Map Text Text)
mboxArcSeal = do
    void $ (string (T.pack "ARC-Seal"))
    void $ S.lexeme (single ':')
    vars <- mboxArcVars
    return (M.fromList vars)

mboxArcVar :: Parser (Text, Text)
mboxArcVar = do
    void $ many spaceChar
    key <- S.lexeme (some alphaNumChar)
    void $ S.lexeme (char '=')
    val <- S.lexeme (someTill L.charLiteral (lookAhead (single ';' <|> single '=')))
    let val' = Prelude.filter (isNotSpace) val
    return ((T.pack key), (T.pack val))

isNotSpace :: Char -> Bool
isNotSpace '\n' = False
isNotSpace ' ' = False
isNotSpace '\t' = False
isNotSpace _ = True

mboxArcVars :: Parser [(Text, Text)]
mboxArcVars = do
    arcVars <- endBy1 (sepBy mboxArcVar (single ';') ) (string (T.pack "=="))
    return (Prelude.foldr (++) [] arcVars)

