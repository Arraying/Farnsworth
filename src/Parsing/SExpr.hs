module Parsing.SExpr
    ( SExpr (..)
    , parseStr
    ) where

import           Control.Applicative    (many, (<|>))
import           Control.Monad          (void)
import           Data.Char              (isDigit, isLetter)
import           Data.List              (intercalate)
import           Errors
import qualified Text.Parsec            as P
import qualified Text.Parsec.Char       as Char
import qualified Text.Parsec.Combinator as Comb
import           Text.Parsec.String     (Parser)

-- The main S-expression type.
data SExpr
  = SNum Integer
  | SSym String
  | SList [SExpr]

-- Custom show instance to create friendly syntax when printing.
instance Show SExpr where
  show (SNum n)  = show n
  show (SSym s)  = s
  show (SList l) = "(" ++ (intercalate " " (map show l)) ++ ")"

expr :: Parser SExpr
expr = num <|> sym <|> list

num :: Parser SExpr
num = do
  n <- lexeme $ Comb.many1 Char.digit
  return $ SNum $ read n

sym :: Parser SExpr
sym = lexeme $ do
  fc <- headParse
  rest <- many tailParse
  return $ SSym $ fc : rest
  where
    headParse = Char.satisfy allowed
    tailParse = Char.satisfy (\a -> isDigit a || allowed a)
    allowed a = isLetter a || elem a "!$%^&*-_=+;:@'#~|<>/\\"

list :: Parser SExpr
list = do
  void $ lexeme $ Char.char '('
  e <- many expr
  void $ lexeme $ Char.char ')'
  return $ SList e

parseStr :: String -> Either FWError SExpr
parseStr str = mapLeft (\e -> FWParseError e) $ P.parse ((whitespace *> expr) <* Comb.eof) "" str

whitespace :: Parser ()
whitespace = void $ many $ Char.oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

mapLeft :: (a -> b) -> Either a r -> Either b r
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x
