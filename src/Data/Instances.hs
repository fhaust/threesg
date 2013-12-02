


module Data.Instances where

import Control.DeepSeq
import System.Random

import Linear

--------------------------------------------------
-- Orphan Instances

instance NFData a => NFData (V3 a) where
  rnf (V3 x y z) = rnf x `seq` rnf y `seq` rnf z

instance Random a => Random (V3 a) where
  random g0 = (V3 x y z, g3)
    where (x,g1) = random g0
          (y,g2) = random g1
          (z,g3) = random g2
  randomR (V3 lox loy loz, V3 hix hiy hiz) g0 = (V3 x y z, g3)
    where (x,g1) = randomR (lox,hix) g0
          (y,g2) = randomR (loy,hiy) g1
          (z,g3) = randomR (loz,hiz) g2


