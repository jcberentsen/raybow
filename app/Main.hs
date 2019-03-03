{-# language OverloadedStrings #-}
module Main where

-- import Raybow
import Horizon

import Prelude hiding (concat)
import Graphics.Gloss
-- import Graphics.Gloss.Data.Bitmap
import Data.ByteString (pack, concat)
import Data.Bits

main :: IO ()
main = do
  let window = InWindow "Raybow" (200,200) (10,400)
      background = greyN 0.4

  simulate
    window
    background
    100
    Horizon.initialModel
    view
    step

view :: Horizon.Model -> Picture
view model =
  let bs = (concat . map pack) $
        Horizon.toPixels model
      bitmapFormat = BitmapFormat TopToBottom PxRGBA
      rendering = bitmapOfByteString 1 2 bitmapFormat bs True
  in
    scale 100 200 rendering

step :: b1 -> b2 -> Horizon.Model -> Horizon.Model
step = const (const (\(Horizon.Model (sky, ground, horizon)) -> Horizon.Model ((sky + 1) .&. 255, ground, horizon)))
