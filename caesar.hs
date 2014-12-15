import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

let2Upperint :: Char -> Int
let2Upperint c = ord c - ord 'A'

int2Upperlet :: Int -> Char
int2Upperlet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2Upperlet ((let2Upperint c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
