-- | Project Euler 6: квадрат суммы минус сумма квадратов.
module Euler.Problem06 (
  sumSquareTail,
  sumSquareRecursive,
  sumSquareModular,
  sumSquareMap,
  sumSquareComprehension,
  sumSquareInfinite,
  variants,
) where

import Data.List (foldl')

-- | Хвостовая рекурсия с аккумуляторами
sumSquareTail :: Integer -> Integer
sumSquareTail n = go 1 0 0
 where
  go i total squares
    | i > n = total * total - squares
    | otherwise = go (i + 1) (total + i) (squares + i * i)

-- | Обычная рекурсия
sumSquareRecursive :: Integer -> Integer
sumSquareRecursive n = total * total - sumSquares n
 where
  total = sumTo n
  sumTo k
    | k <= 0 = 0
    | otherwise = k + sumTo (k - 1)
  sumSquares k
    | k <= 0 = 0
    | otherwise = k * k + sumSquares (k - 1)

-- | Генерация, фильтрация, свёртка
sumSquareModular :: Integer -> Integer
sumSquareModular n = foldl' (+) 0 products
 where
  numbers = [1 .. n]
  pairs = [(i, j) | i <- numbers, j <- numbers]
  products = map (uncurry (*)) (filter (uncurry (/=)) pairs)

-- | Отображение map
sumSquareMap :: Integer -> Integer
sumSquareMap n = total * total - sum squares
 where
  numbers = [1 .. n]
  squares = map (\x -> x * x) numbers
  total = sum numbers

-- | Списковое включение
sumSquareComprehension :: Integer -> Integer
sumSquareComprehension n = total * total - sum [x * x | x <- [1 .. n]]
 where
  total = sum [1 .. n]

-- | Бесконечный список
sumSquareInfinite :: Integer -> Integer
sumSquareInfinite n = total * total - sum (map (\x -> x * x) numbers)
 where
  numbers = takeWhile (<= n) [1 ..]
  total = sum numbers

variants :: [(String, Integer -> Integer)]
variants =
  [ ("tail", sumSquareTail)
  , ("recursive", sumSquareRecursive)
  , ("modular", sumSquareModular)
  , ("map", sumSquareMap)
  , ("comprehension", sumSquareComprehension)
  , ("infinite", sumSquareInfinite)
  ]
