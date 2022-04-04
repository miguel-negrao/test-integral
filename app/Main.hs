{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- base
import Control.Monad (forM_)
import Control.Monad.Trans.MSF
import Data.Functor.Identity
import Data.MonadicStreamFunction
import FRP.BearRiver
import Text.Printf

main :: IO ()
main = forM_ [100, 1000, 10000] printErrorFull

{--
plots:
https://www.wolframalpha.com/input?i=plot+x%5E3+from+0+to+10
https://www.wolframalpha.com/input?i=plot+-+x**3+%2B+10+x**2+%2B+4+x+from+0+to+15
https://www.wolframalpha.com/input?i=plot+%28e+**+%28-++x+%2F+10%29++*+cos+%28x%29+%29+from+0+to+10

primitive:
https://www.wolframalpha.com/input?i=antiderivative+-+x**3+%2B+10+x**2+%2B+4+x%2B+1

definite integral:
https://www.wolframalpha.com/input?i=integral+%28+%28e+**+%28-++x+%2F+10%29+%29+*+cos+%28x%29+%29+from+0+to+t
--}


printErrorFull :: Int -> IO ()
printErrorFull numPoints = do
  printf "\nnumPoints: %d\n" numPoints
  printErrorHeader
  mapM_
    (printError numPoints)
    [ ("f(x) = 1", const 1, id, 10),
      ("f(x) = x", id, \x -> (x ** 2) / 2, 10),
      ("f(x) = x**2", (** 2), \x -> (x ** 3) / 3, 10),
      ("f(x) = x**3", (** 3), \x -> (x ** 4) / 4, 10),
      ("f(x) = -x**3+10 x**2+4 x+1",
        \x -> - (x ** 3) + 10 * (x ** 2) + 4 * x, 
        \x -> - (x ** 4) / 4 + 10 * (x ** 3) / 3 + 2 * (x ** 2), 15),
      ("f(x) = sin(x)", sin, \x -> 1 - cos x, 4 * pi),
      ("f(x) = e^x", exp, \x -> exp x - 1, 2),
      ("(e**(-x/10))* cos(x)", \x -> exp (- x / 10) * cos x,
        \x -> 10.0 / 101.0 * (1 - exp (- x / 10) * (cos x - 10 * sin x)), 8)
    ]

integralTrap :: (Monad m, VectorSpace a s) => SF m a a
integralTrap = integralTrapFrom zeroVector

integralTrapFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
integralTrapFrom a0 =
  zeroVector --> proc a -> do
    dt <- constM ask -< ()
    aPrev <- iPre zeroVector -< a
    accumulateWith (^+^) a0 -< (realToFrac dt / 2) *^ (a ^+^ aPrev)

printErrorHeader :: IO ()
printErrorHeader = printf "%30s: %10s %10s %10s %10s\n" ("Description" :: String) ("Trap bttr" :: String) ("MSE Rect" :: String) ("MSE Trap" :: String) ("ratio" :: String)

printError :: Int -> (String, Time -> Double, Time -> Double, Time) -> IO ()
printError numPoints (name, f, integralF, duration) = putStrLn string
  where
    string = printf "%30s: %10s %10.3e %10.3e %10.3e" name (show rectVsTrap) errorRect errorTrap (errorRect / errorTrap)
    meanSquaredError cell = sum (runIdentity $ embed (runReaderS $ errorCell f integralF cell) values) / fromIntegral size
    errorRect = meanSquaredError integral
    errorTrap = meanSquaredError integralTrap
    rectVsTrap = errorRect >= errorTrap
    values = (0.0, ()) : zip (replicate numPoints (duration / fromIntegral numPoints)) (repeat ())
    size = length values

errorCell ::
  (Time -> Double) ->
  (Time -> Double) ->
  MSF (ClockInfo Identity) Double Double ->
  MSF (ClockInfo Identity) () Double
errorCell f integralF integralCell = proc () -> do
  t <- time -< ()
  fValue <- arr f -< t
  exactIntegral <- arr integralF -< t
  numericIntegral <- integralCell -< fValue
  returnA -< (exactIntegral - numericIntegral) ** 2
