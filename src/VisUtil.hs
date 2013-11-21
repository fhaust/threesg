
{-# LANGUAGE FlexibleInstances #-}

module VisUtil where


import Data.OFF
import Data.Octree

import Vis
import Linear


import Control.Lens


class Picturable a where
  toPicture :: a -> VisObject Double




instance Picturable (Octree Vertex) where
  toPicture (Leaf _) = VisObjects []
  toPicture oct      = VisObjects $ (Trans origin $ Box (size^._x, size^._y, size^._z) Wireframe red)
                                  : map (toPicture . ($ oct)) [mmm, mmp, mpm, mpp, pmm, pmp, ppm, ppp]
                                  
    where (minB, maxB) = bounds oct                                  
          origin       = minB + (size ^* 0.5)
          size         = maxB - minB

                                  
