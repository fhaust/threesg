
{-# LANGUAGE FlexibleInstances #-}

module VisUtil where


import Data.OFF

import Vis
import Linear


import Control.Lens


class Picturable a where
  toPicture :: a -> VisObject Double




