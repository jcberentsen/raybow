module Horizon
    ( Model(..)
    , initialModel
    , toPixels
    , groundTruth
    , step
    ) where

import Data.Word
import Data.Bits

-- |  A dummy model
-- | ( sky  -- luminance
-- | , ground -- luminance
-- | , horizon -- placement of horizon along y axis
-- | )

newtype Model = Model (Word8, Word8, Integer)
type Image = [[Word8]]

initialModel :: Model
initialModel = Model (50, 20, 0)

-- | An Image in the empirical distribution (i.e. an actual Image of a Horizon)
groundTruth :: Image
groundTruth =
    [ [0, 250, 250, 255]
    , [80, 80, 0, 255]
    ]

toPixels :: Model -> Image
toPixels (Model (sky, ground, _)) =
  [ [0, sky, sky, 255]
  , [ground, ground, 0, 255]
  ]

imageError :: Image -> Image -> Image
imageError estimage real =
  let pixelError (pe, pr) = zipWith (-) pe pr
  in
    map pixelError $ zip estimage real

step :: Model -> Model
step (Horizon.Model (sky, ground, horizon))
  = Horizon.Model ((sky + 1) .&. 255, ground, horizon)
-- TODO use groundTruth for dM computation
