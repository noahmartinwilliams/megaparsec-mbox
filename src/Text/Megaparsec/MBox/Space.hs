module Text.Megaparsec.MBox.Space where

import Text.Megaparsec
import Data.Text as T
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.MBox.Types

sc :: MBoxParser ()
sc = L.space space1 Text.Megaparsec.empty Text.Megaparsec.empty

lexeme :: MBoxParser a -> MBoxParser a
lexeme = L.lexeme sc

