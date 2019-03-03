{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImageTest where

import Horizon
import Data.Word
import Numeric.AD
import Numeric.AD.Jet

import Test.Tasty.HUnit

-- HUnit test case
unit_imageErrorOnEmptyImages :: IO ()
unit_imageErrorOnEmptyImages = imageError [] [] @?= []

unit_imageErrorOnOnePixel :: IO ()
unit_imageErrorOnOnePixel = imageError [[1]] [[0 :: Word8]] @?= [[1]]

unit_forwardError :: IO ()
unit_forwardError =
  let model = Horizon.initialModel
      forward = toPixels model
      forwardError = imageError groundTruth forward
  in
    forwardError @?= [ [  0, 200, 200, 0]
                     , [ 60,  60,   0, 0]
                     ]


-- https://www.stackage.org/lts-12.19/package/ad-4.3.5
unit_ad :: IO ()
unit_ad =
  diff' (exp . log) 2 @?= (2, 1 :: Double)

assertNear :: Double -> Double -> Assertion
assertNear = assertEpsilonNear 0.0001

assertEpsilonNear :: (Show a, Ord a, Num a) => a -> a -> a -> Assertion
assertEpsilonNear eps expected actual=
  assertBool (show actual ++ " isn't near " ++ show expected) (actual < expected + eps && actual > expected - eps)

unit_adMulti :: IO ()
unit_adMulti =
  let df = headJet $ jet $ grads (\[x,y] -> exp (x * y)) [1, 2 :: Double]
  in assertEpsilonNear 0.01 7.389 df

--
-- unit_adImageError :: IO ()
-- unit_adImageError =
--   diff' (imageError groundTruth . toPixels) Horizon.initialModel @?= ([], [])
