module Text.Megaparsec.MBox.Space where

import Text.Megaparsec
import Data.Text as T
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.MBox.Types

sc :: MBoxParser ()
sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

hsc :: MBoxParser ()
hsc = L.space hspace1 Text.Megaparsec.empty Text.Megaparsec.empty

lexeme :: MBoxParser a -> MBoxParser a
lexeme = L.lexeme sc

hlexeme :: MBoxParser a -> MBoxParser a
hlexeme = L.lexeme hsc

isNotSpace :: Char -> Bool
isNotSpace '\n' = False
isNotSpace ' ' = False
isNotSpace '\t' = False
isNotSpace _ = True

