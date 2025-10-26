{-# LANGUAGE BangPatterns #-}

import Control.Monad.State.Strict (State, get, put, runState)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
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

topo :: Value -> [Value]
topo root = order
  where
    (order, _) = dfs root [] IS.empty
    dfs :: Value -> [Value] -> IS.IntSet -> ([Value], IS.IntSet)
    dfs v agg seen
      | nodeId v `IS.member` seen = (agg, seen)
      | otherwise = (v : agg', seen'')
      where
        seen' = IS.insert (nodeId v) seen
        (agg', seen'') =
          foldl'
            (\(agg0, seen0) p -> dfs p agg0 seen0)
            (agg, seen')
            (parents v)

backward :: Value -> IM.IntMap Double
backward out = foldl' step grads0 (topo out)
  where
    grads0 = IM.singleton (nodeId out) 1.0
    step :: IM.IntMap Double -> Value -> IM.IntMap Double
    step grads v = foldl' (\g (vid, gadd) -> IM.insertWith (+) vid gadd g) grads contribs
      where
        delCurr = IM.findWithDefault 0 (nodeId v) grads
        contribs = localBack v delCurr

-- mimics Kapathy's python micrograd example
-- https://github.com/karpathy/micrograd
buildExpr :: Gen Value
buildExpr = do
  a <- newValue (-4.0)
  b <- newValue 2
  c <- a `addV` b

  ab <- a `multV` b
  bPow3 <- b `powConstV` 3
  d <- ab `addV` bPow3

  cc <- c `addV` c
  one1 <- newValue 1
  c' <- cc `addV` one1

  one2 <- newValue 1
  na <- negV a
  tmpC'' <- one2 `addV` c'
  tmpC''2 <- c' `addV` tmpC''
  c'' <- tmpC''2 `addV` na

  twoD <- scalarMultV 2 d
  ba <- b `addV` a
  reluBA <- reluV ba
  tmpD' <- d `addV` twoD
  d' <- tmpD' `addV` reluBA

  threeD' <- scalarMultV 3 d'
  bMinusA <- b `subV` a
  reluBminusA <- reluV bMinusA
  tmpD'' <- d' `addV` threeD'
  d'' <- tmpD'' `addV` reluBminusA

  e <- c'' `subV` d''
  f <- e `powConstV` 2

  two <- newValue 2
  g <- f `divV` two

  ten <- newValue 10
  tenOverF <- ten `divV` f
  g `addV` tenOverF

main :: IO ()
main = do
  let (outNode, _) = runState buildExpr 0
  print outNode
  let grads = backward outNode
  print grads