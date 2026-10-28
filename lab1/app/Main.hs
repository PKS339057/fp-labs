module Main (main) where

import qualified Euler.Problem06 as P06
import qualified Euler.Problem15 as P15

main :: IO ()
main = do
  mapM_ (\(name, solve) -> putStrLn ("6 " ++ name ++ " " ++ show (solve 100))) P06.variants
  mapM_ (\(name, solve) -> putStrLn ("15 " ++ name ++ " " ++ show (solve 20))) P15.variants
