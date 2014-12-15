module Parsing where

import Data.Char
import Control.Monad

infixr 5 +++

newtype Parser a =  P (String -> [(a,String)])

instance Monad Parser where
    return v =  P (\inp -> [(v,inp)])
    p >>= f = P (\ inp -> case parse p inp of
      [] -> []
      [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
    mzero =  P (\inp -> [])
    p `mplus` q =  P (\inp -> case parse p inp of
      []        -> parse q inp
      [(v,out)] -> [(v,out)])

failure :: Parser a
failure =  mzero

item :: Parser Char
item =  P (\inp -> case inp of
  []     -> []
  (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q =  p `mplus` q

sat :: (Char -> Bool) -> Parser Char
sat p =  do x <- item
            if p x then
              return x
            else failure

digit :: Parser Char
digit =  sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum =  sat isAlphaNum

char :: Char -> Parser Char
char x =  sat (== x)

string :: String -> Parser String
string []      =  return []
string (x:xs)  =  do char x
                     string xs
                     return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p =  do v <- p
              vs <- many p
              return (v:vs)

ident :: Parser String
ident =  do x <- lower
            xs <- many alphanum
            return (x:xs)

nat :: Parser Double
nat =  do xs <- many1 digit
          return (read xs)

int :: Parser Double
int = (do symbol "-"
          e <- natural
          return (-e)) +++ natural
 
space :: Parser ()
space =  do many (sat isSpace)
            return ()

comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             return ()

token :: Parser a -> Parser a
token p =  do space
              v <- p
              space
              return v

identifier :: Parser String
identifier =  token ident

natural :: Parser Double
natural =  token nat

integer :: Parser Double
integer =  token int

symbol :: String -> Parser String
symbol xs =  token (string xs)

expr :: Parser Double
expr = do t <- term
          addition t +++ subtraction t +++ return t
          where addition t = do symbol "+"
                                e <- expr
                                return (t + e)
                subtraction t = do symbol "-"
                                   f <- expr
                                   return (t - f)
              

term :: Parser Double
term = do f <- factor
          exponential f +++ multiplication f +++ division f +++ return f
          where exponential f = do symbol "**"
                                   t <- term
                                   return (f ** t)
                multiplication f = do symbol "*"
                                      t <- term
                                      return (f * t)
                division f = do symbol "/"
                                t <- term
                                return (f / t)

factor :: Parser Double
factor = parens +++ integer
         where parens = do symbol "("
                           e <- expr
                           symbol ")"
                           return e

eval :: String -> Double
eval xs = case parse expr xs of
            [(n, [])] -> n
            [(_,out)] -> error("unused input" ++ out)
            [] -> error "invalid input"
