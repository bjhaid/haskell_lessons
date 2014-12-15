import Prelude hiding ((^), and, takeWhile, return)
m ^ 1 = m
m ^ n = m * (^) m (n - 1)

and [] = True
and (b : bs) = and bs && b

takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = []

dec2int' = foldl (\ x y -> y + 10 * x) 0

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \ inp -> [(v, inp)]

failure :: Parser a
failure = \ inp -> []

item :: Parser Char
item = \ inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

p +++ q = \ inp -> case p inp of
  [] -> parse q inp
  [(v,out)] -> [(v,out)]

(>>=) :: Parser a -> (a -> Parser b) -> Parser b

p >>= f = \ inp -> case parse p inp of
  [] -> []
  [(v,out)] -> parse (f v) out
