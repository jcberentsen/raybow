{-# LANGUAGE ScopedTypeVariables #-}

module ImageTest where

import Horizon
import Data.Word

import Test.Tasty.HUnit

-- HUnit test case
unit_imageErrorOnEmptyImages :: IO ()
unit_imageErrorOnEmptyImages = imageError [] [] @?= []

unit_imageErrorOnOnePixel :: IO ()
unit_imageErrorOnOnePixel = imageError [[1]] [[0 :: Word8]] @?= [[1]]
