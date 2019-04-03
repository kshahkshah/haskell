{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = mod x 10 : toDigitsRev (div x 10)
  | otherwise = []

enumerate :: [Integer] -> [(Integer, Integer)]
enumerate x = zip [0..] x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  [((mod (i + indexShift - 1) 2) + 1) * x  | (i, x) <- enumerate xs, let indexShift = toInteger (mod (length xs) 2)]

sumDigits :: [Integer] -> Integer
sumDigits xs =
  foldl (\total n -> total + n) 0 (
    foldl (\acc x -> acc ++ toDigits x) [] xs
  )

validate :: Integer -> Bool
validate cardNumber =
  mod (sumDigits (doubleEveryOther (toDigits cardNumber))) 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end temp
  | n <= 0    = []
  | n == 1    = [(start,end)]
  | otherwise = hanoi (n-1) start temp end ++ hanoi 1 start end temp ++ hanoi (n-1) temp end start