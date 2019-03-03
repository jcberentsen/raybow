module Horizon
    ( Model(..)
    , initialModel
    , toPixels
    , groundTruth
    , imageError
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

-- | NOTE: the real Image and the estimated Image seems to diverge
-- | in semantics here, and if swapped, yields wrong result
-- | maybe there is something the type system can be used for
-- | to ensure the quality of this?
-- | The Real Image somehow has a higher quality of information
-- | (reminds of propagator information lattice, maybe not too relevant)
-- | NOTE: Or maybe Error? should be a type encapsulating Real `sub` Estimate?
-- | perhaps also eventually accomodating differentials
imageError :: Image -> Image -> Image
imageError real estimate =
  let pixelError (pe, pr) = zipWith (-) pr pe
  in
    map pixelError $ zip estimate real

step :: Model -> Model
step model@(Horizon.Model (sky, ground, horizon)) =
  let forward = toPixels model
      forwardError = imageError groundTruth forward
      -- backprop to delta Model
      -- scale v = fromIntegral $ (fromIntegral v) `div` (128 :: Word8)
      -- delta = map (map scale) forwardError
      -- backprop

  in
  Horizon.Model ((sky + 1) .&. 255, ground, horizon)
