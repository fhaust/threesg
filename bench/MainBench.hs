

import Criterion.Main

import Data.OFF

import qualified Data.Octree as Oct
import qualified Data.KDTree as KD 

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.List as L

import qualified Data.Foldable as F

import Data.Function

import Control.DeepSeq
import Control.Applicative

import System.Random

import Linear



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


main :: IO ()
main = do

  -- my test data set ... kind of personal ... ie ... my face :)
  --(Right off) <- readCNOFF "data/face_point_set.cnoff"
  --let (vs,ns,cs) = V.unzip3 off 

  -- create 500k samples
  gen <- getStdGen
  let vs = V.fromListN 5000 $ randoms gen :: V.Vector (V3 Double)

  

  -- create kdtree from dataset
  let kd = force . KD.kdtree 8 . V.convert $ vs


  -- run benchmarks
  defaultMain

     -- search the nearest neighbor 
     [ bench  "linear"     $ nf (\p -> V.minimumBy (compare `on` qd p) vs) 0
     , bench  "linear_all" $ nf (\p -> L.take 5 . L.sortBy (compare `on` qd p) . V.toList $ vs) 0 
     , bench  "kdtree"     $ nf (KD.nearestNeighbor kd) 0 
     , bench  "kdtree_all" $ nf (L.take 5 . KD.nearestNeighbors kd) 0
     ]
