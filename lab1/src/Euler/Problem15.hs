{- | Project Euler 15: маршруты по сетке n×n клеток, шаги только вправо и вниз.
Маршрут — 2n шагов, из них n вправо, поэтому ответ C(2n, n) = (n+1)...(2n) / n!.
-}
module Euler.Problem15 (
  pathsTail,
  pathsRecursive,
  pathsModular,
  pathsMap,
  pathsComprehension,
  pathsInfinite,
  variants,
) where

import Data.List (foldl')

-- | 1. Хвостовая рекурсия: числитель и знаменатель копятся в аккумуляторах.
pathsTail :: Integer -> Integer
pathsTail n = go 1 1 1
 where
  go i top bottom
    | i > n = top `div` bottom
    -- seq, как в задаче 6: вычислить аккумуляторы сразу, не копить отложенные умножения.
    | otherwise = top `seq` bottom `seq` go (i + 1) (top * (n + i)) (bottom * i)

-- | 2. Обычная рекурсия: умножение выполняется после возврата из вызова.
pathsRecursive :: Integer -> Integer
pathsRecursive n = productFrom (n + 1) (2 * n) `div` productFrom 1 n
 where
  productFrom a b
    | a > b = 1
    | otherwise = a * productFrom (a + 1) b

{- | 3. Генерация -> фильтрация -> свёртка.
Фильтр делит числа 1..2n: большие идут в числитель, малые — в знаменатель.
-}
pathsModular :: Integer -> Integer
pathsModular n = multiply (filter (> n) numbers) `div` multiply (filter (<= n) numbers)
 where
  numbers = [1 .. 2 * n]
  multiply = foldl' (*) 1

-- | 4. Множители числителя n+1..2n получаются отображением map.
pathsMap :: Integer -> Integer
pathsMap n = product (map (n +) [1 .. n]) `div` product [1 .. n]

-- | 5. Списковое включение вместо цикла.
pathsComprehension :: Integer -> Integer
pathsComprehension n = product [n + i | i <- [1 .. n]] `div` product [1 .. n]

{- | 6. Бесконечный треугольник Паскаля: C(2n, n) — середина строки 2n.
Ленивость: из бесконечного списка строк вычисляются только первые 2n+1.
-}
pathsInfinite :: Integer -> Integer
pathsInfinite n = pascal !! fromInteger (2 * n) !! fromInteger n
 where
  pascal = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

variants :: [(String, Integer -> Integer)]
variants =
  [ ("tail", pathsTail)
  , ("recursive", pathsRecursive)
  , ("modular", pathsModular)
  , ("map", pathsMap)
  , ("comprehension", pathsComprehension)
  , ("infinite", pathsInfinite)
  ]
