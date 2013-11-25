

import Criterion.Main

import Data.OFF

import qualified Data.LinSearch as Lin
import qualified Data.KDTree as KD 

import qualified Data.Vector as V
import qualified Data.List as L

import Control.DeepSeq

import Data.Instances ()



main :: IO ()
main = do

  -- my test data set ... kind of personal ... ie ... my face :)
  Right off <- readCNOFF "data/face_point_set.cnoff"
  let (vs,ns,cs) = V.unzip3 off 



  -- create some samples
  --gen <- getStdGen
  --let vs = V.fromListN 500000 $ randoms gen :: V.Vector (V3 Double)

  -- get a query point that exists in the set
  let q = vs V.! (V.length vs `quot` 2)
  

  -- create kdtree from dataset
  let kd = force . KD.kdtree 8 . V.convert $ vs


  -- run benchmarks
  defaultMain

     -- search the nearest neighbor 
     [ bgroup "nn" 
       [ bench  "linear_nn"  $ nf (Lin.nearestNeighbor vs) q
       , bench  "kdtree_nn"  $ nf (KD.nearestNeighbor kd) q 
       ]
     , bgroup "nn5"
       [ bench  "linear_nn5" $ nf (L.take 5 . Lin.nearestNeighbors vs) q 
       , bench  "kdtree_nn5" $ nf (L.take 5 . KD.nearestNeighbors kd) q
       ]
     , bgroup "nr"
       [ bench "linear_nr" $ nf (Lin.pointsAround vs 1) q
       , bench "kdtree_nr" $ nf (KD.pointsAround kd 1) q
       ]
     ]

