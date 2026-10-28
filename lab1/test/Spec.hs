module Main (main) where

import Control.Monad (unless)
import qualified Euler.Problem06 as P06
import qualified Euler.Problem15 as P15
import System.Exit (exitFailure)

type Check = (String, Bool)

main :: IO ()
main = do
  let failed = [name | (name, passed) <- checks, not passed]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " checks passed")
  unless (null failed) exitFailure

checks :: [Check]
checks = known06 ++ formula06 ++ known15 ++ oracle15

-- Задача 6: известные ответы
known06 :: [Check]
known06 =
  [ ("6/" ++ name ++ "/" ++ show n, solve n == expected)
  | (name, solve) <- P06.variants
  , (n, expected) <- [(0, 0), (1, 0), (2, 4), (10, 2640), (100, 25164150), (-5, 0)]
  ]

-- Задача 6: сверка с формулами
formula06 :: [Check]
formula06 =
  [ ("6/formula/" ++ name ++ "/" ++ show n, solve n == expected n)
  | (name, solve) <- P06.variants
  , n <- [1 .. 150]
  ]
 where
  expected k = total * total - k * (k + 1) * (2 * k + 1) `div` 6
   where
    total = k * (k + 1) `div` 2

-- Задача 15: известные ответы
known15 :: [Check]
known15 =
  [ ("15/" ++ name ++ "/" ++ show n, solve n == expected)
  | (name, solve) <- P15.variants
  , (n, expected) <-
      [(0, 1), (1, 2), (2, 6), (3, 20), (20, 137846528820), (40, 107507208733336176461620)]
  ]

-- Задача 15: сверка прямым перебором
oracle15 :: [Check]
oracle15 =
  [ ("15/oracle/" ++ name ++ "/" ++ show n, solve n == walk n n)
  | (name, solve) <- P15.variants
  , n <- [0 .. 6]
  ]
 where
  walk :: Integer -> Integer -> Integer
  walk 0 _ = 1
  walk _ 0 = 1
  walk x y = walk (x - 1) y + walk x (y - 1)
