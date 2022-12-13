module AdventParser where

import Control.Applicative (Applicative (liftA2))
import Text.Parsec (digit, many1, option, string)
import Text.Parsec.String (Parser)

integer :: Parser Int
integer = read <$> many1 digit

signedInteger :: Parser Int
signedInteger = read <$> liftA2 (++) (option "" (string "-")) (many1 digit)
