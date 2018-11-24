{-# language OverloadedStrings #-}
module Main where

import Lib

import Prelude hiding (concat)
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Data.ByteString (pack, concat)
import Data.Bits

main :: IO ()
main = do

  let window = InWindow "Raybow" (200,200) (10,400)
  let background = greyN 0.4

  -- TODO Adjust bitmap to window
  -- window background picture
  simulate window background 100 initialModel view step

initialModel = 0

view x =
  let inv = 255 - x
      bs = (concat . map pack) $
        [ [0, x, x, 255]
        , [0, inv, inv, 255]
        ]
      bitmapFormat = BitmapFormat TopToBottom PxRGBA
      rendering = bitmapOfByteString 2 1 bitmapFormat bs True
  in
    scale 100 200 rendering

step = const (const (\x -> (x + 1) .&. 255))
