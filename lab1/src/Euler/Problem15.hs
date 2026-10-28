-- | Project Euler 15: число маршрутов по сетке n×n, ответ C(2n, n).
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

-- | Хвостовая рекурсия с аккумуляторами
pathsTail :: Integer -> Integer
pathsTail n = go 1 1 1
 where
  go i top bottom
    | i > n = top `div` bottom
    | otherwise = go (i + 1) (top * (n + i)) (bottom * i)

-- | Обычная рекурсия
pathsRecursive :: Integer -> Integer
pathsRecursive n = productFrom (n + 1) (2 * n) `div` productFrom 1 n
 where
  productFrom a b
    | a > b = 1
    | otherwise = a * productFrom (a + 1) b

-- | Генерация, фильтрация, свёртка
pathsModular :: Integer -> Integer
pathsModular n = multiply (filter (> n) numbers) `div` multiply (filter (<= n) numbers)
 where
  numbers = [1 .. 2 * n]
  multiply = foldl' (*) 1

-- | Отображение map
pathsMap :: Integer -> Integer
pathsMap n = product (map (n +) [1 .. n]) `div` product [1 .. n]

-- | Списковое включение
pathsComprehension :: Integer -> Integer
pathsComprehension n = product [n + i | i <- [1 .. n]] `div` product [1 .. n]

-- | Бесконечный треугольник Паскаля
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
