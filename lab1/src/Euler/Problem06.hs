-- | Project Euler 6: квадрат суммы минус сумма квадратов чисел от 1 до n.
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

-- | 1. Хвостовая рекурсия: сумма и сумма квадратов копятся в аккумуляторах.
sumSquareTail :: Integer -> Integer
sumSquareTail n = go 1 0 0
 where
  go i total squares
    | i > n = total * total - squares
    -- seq вычисляет аккумуляторы сразу, иначе копится цепочка отложенных сложений.
    | otherwise = total `seq` squares `seq` go (i + 1) (total + i) (squares + i * i)

-- | 2. Обычная рекурсия: сложение выполняется после возврата из вызова.
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

{- | 3. Генерация -> фильтрация -> свёртка.
(1 + ... + n)^2 — это сумма произведений i * j по всем парам (i, j).
Пары с i == j дают сумму квадратов, поэтому оставляем только пары с i /= j.
-}
sumSquareModular :: Integer -> Integer
sumSquareModular n = foldl' (+) 0 products
 where
  numbers = [1 .. n]
  pairs = [(i, j) | i <- numbers, j <- numbers]
  products = map (uncurry (*)) (filter (uncurry (/=)) pairs)

-- | 4. Последовательность квадратов получается отображением map.
sumSquareMap :: Integer -> Integer
sumSquareMap n = total * total - sum squares
 where
  numbers = [1 .. n]
  squares = map (\x -> x * x) numbers
  total = sum numbers

{- | 5. Циклов for/while в Haskell нет: нет изменяемой переменной-счётчика.
Ближе всего к циклу по записи списковое включение: «x * x для каждого x из [1..n]».
-}
sumSquareComprehension :: Integer -> Integer
sumSquareComprehension n = total * total - sum [x * x | x <- [1 .. n]]
 where
  total = sum [1 .. n]

-- | 6. Бесконечный список [1 ..]: благодаря ленивости вычисляется только префикс до n.
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
