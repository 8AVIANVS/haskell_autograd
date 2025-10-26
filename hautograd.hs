{-# LANGUAGE BangPatterns #-}

import Control.Monad.State.Strict (State, get, put, runState)
import Control.Monad.State.Strict qualified as ST
import Data.IntMap.Strict qualified as IM
import Data.List (foldl')

data Value = Value
  { nodeId :: !Int, -- forcing strict field here
    data' :: !Double,
    parents :: [Value],
    localBack :: Double -> [(Int, Double)],
    _op :: String
  }

instance Show Value where
  show :: Value -> String
  show (Value vid d _ _ o) = "ValueID: " ++ show vid ++ " {\n  Data: " ++ show d ++ "\n  Op: " ++ show o ++ "\n}"

type Gen a = State Int a

freshId :: Gen Int
freshId = do
  !i <- get
  put $! (i + 1)
  return i

newValue :: (Real a) => a -> Gen Value
newValue n = do
  i <- freshId
  return
    Value
      { nodeId = i,
        data' = realToFrac n,
        parents = [],
        localBack = const [],
        _op = "leaf"
      }

addV :: Value -> Value -> Gen Value
addV x y = do
  i <- freshId
  let outData = data' x + data' y
      fBack delCurr =
        [(nodeId x, delCurr), (nodeId y, delCurr)]
  return
    Value
      { nodeId = i,
        data' = outData,
        parents = [x, y],
        localBack = fBack,
        _op = "+"
      }

multV :: Value -> Value -> Gen Value
multV x y = do
  i <- freshId
  let outData = data' x * data' y
      fBack delCurr =
        [(nodeId x, delCurr * data' y), (nodeId y, delCurr * data' x)]
  return
    Value
      { nodeId = i,
        data' = outData,
        parents = [x, y],
        localBack = fBack,
        _op = "*"
      }

scalarMultV :: (Real a) => a -> Value -> Gen Value
scalarMultV c v = do
  c' <- newValue c
  c' `multV` v

powConstV :: (Real a) => Value -> a -> Gen Value
powConstV x p = do
  i <- freshId
  let p' = realToFrac p
      outData = data' x ** p'
      fBack delCurr =
        [(nodeId x, delCurr * p' * (data' x ** (p' - 1)))]
  return
    Value
      { nodeId = i,
        data' = outData,
        parents = [x],
        localBack = fBack,
        _op = "**" ++ show p'
      }

reluV :: Value -> Gen Value
reluV x = do
  i <- freshId
  let outData = max 0 (data' x)
      fBack delCurr =
        [(nodeId x, delCurr * signum outData)]
  return
    Value
      { nodeId = i,
        data' = outData,
        parents = [x],
        localBack = fBack,
        _op = "ReLU"
      }

--------------- non-primitive binops below ---------------

negV :: Value -> Gen Value
negV = scalarMultV (-1)

subV :: Value -> Value -> Gen Value
subV x y = do
  ny <- negV y
  x `addV` ny

divV :: Value -> Value -> Gen Value
divV x y = do
  invy <- powConstV y (-1)
  x `multV` invy

--------------- end of non-primitive binops --------------

buildExpr :: Gen Value
buildExpr = do
  a <- newValue (-4.0)
  b <- newValue 2
  c <- a `addV` b
  c `addV` c

main :: IO ()
main = do
  let (outNode, _) = runState buildExpr 0
  print outNode