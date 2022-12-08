module AdventParser where

import Text.Parsec (digit, many1)
import Text.Parsec.String (Parser)

integer :: Parser Int
integer = read <$> many1 digit
