-- | Various geometry related utility functions
module Geometry
    ( pointCircleCollision
    , stepPosition
    , stepPositionBounded
    , fromPolar
    , fromRadians
    , fromSubpixels
    ) where

import SDL.Vect (V2 (V2))
import Globals (screenHeight, screenWidth, subpixels, subvelocity)

import Foreign.C.Types (CInt)

-- | Caluclates whether or not a point exists inside of a circle.
pointCircleCollision :: (Integral a, Ord a) 
    => V2 a     -- ^ The centre of the circle
    -> a        -- ^ The radius of the circle
    -> V2 a     -- ^ The point to test
    -> Bool     -- ^ True if the point is inside the circle, False otherwise
pointCircleCollision (V2 x1 y1) r (V2 x2 y2) = let
    dx = (x1 - x2) `quot` 64
    dy = (y1 - y2) `quot` 64
    r' = r `quot` 64
    in dx * dx + dy * dy < r' * r'

-- | Convert from radians to degrees
fromRadians :: Floating a => a -> a
fromRadians theta = theta * 180 / pi 

-- | Convert from subpixels to pixels
fromSubpixels :: Integral a => a -> a
fromSubpixels a = a `quot` subpixels


-- | calculate catesian coordinates from polar ones
fromPolar :: (RealFrac a, Floating a)
    => CInt        -- ^ The radius
    -> a   -- ^ The angle
    -> V2 CInt  -- ^ Cartesian coordinates
fromPolar r theta = V2 (round (r' * cos theta)) (round (r' * sin theta)) where
    r' = fromIntegral r

-- | calculate new position given velocity and change in time.
-- This function actually doesn't care what units the position and time are: 
-- it's generic. This will also screen wrap when the object goes OOB.
stepPositionBounded :: Integral a
    => a        -- ^ The time
    -> V2 a     -- ^ The velocity
    -> V2 a     -- ^ The position
    -> V2 a     -- ^ The new position
stepPositionBounded dt v@(V2 vx vy) p@(V2 px py) = let
    x = (((vx * dt) `quot` subvelocity) + px) `mod` screenWidth
    y = (((vy * dt) `quot` subvelocity) + py) `mod` screenHeight
    in V2 x y

-- | calculate new position given velocity and change in time.
-- This function actually doesn't care what units the position and time are: 
-- it's generic. 
stepPosition :: Integral a
    => a        -- ^ The time
    -> V2 a     -- ^ The velocity
    -> V2 a     -- ^ The position
    -> V2 a     -- ^ The new position
stepPosition dt v@(V2 vx vy) p@(V2 px py) = let
    x = ((vx * dt) `quot` subvelocity) + px        
    y = ((vy * dt) `quot` subvelocity) + py        
    in V2 x y
