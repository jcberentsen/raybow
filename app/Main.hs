{-# language OverloadedStrings #-}
module Main where

-- import Raybow
import Horizon

import Prelude hiding (concat)
import Graphics.Gloss
import Data.ByteString (pack, concat)

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
    Main.step

view :: Horizon.Model -> Picture
view model =
  let bs = (concat . map pack) $
        Horizon.toPixels model
      bitmapFormat = BitmapFormat TopToBottom PxRGBA
      rendering = bitmapOfByteString 1 2 bitmapFormat bs True
  in
    scale 100 200 rendering

step :: b1 -> b2 -> Horizon.Model -> Horizon.Model
step = pure . pure Horizon.step
