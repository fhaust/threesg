
import Data.OFF 



import Data.List

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Vis
import Linear
import VisUtil

main :: IO ()
main = do

  Right r <- readCNOFF "data/face_point_set.cnoff"

  let (vs,ns,cs) = V.unzip3 r 


  let vs' = V.fromList $ fixSize . F.toList $ vs
  
  --displayPointCloudColor (F.toList vs) (F.toList cs)
  --displayPointCloud (F.toList vs) white
  --displayPointCloud vs white
  
  display Nothing "Pointcloud!" $ 
    VisObjects [ Points (fixSize . F.toList$ vs') Nothing white 
               ]





displayPointCloud :: F.Foldable f => f Vertex -> Color -> IO ()
displayPointCloud vs c = display Nothing "Pointcloud!" $ Points (fixSize . F.toList$ vs) Nothing c

displayPointCloudColor :: [Vertex] -> [Color] -> IO ()
displayPointCloudColor vs cs = display Nothing "Pointcloud!" 
                             . VisObjects 
                             $ zipWith (\p c -> Points [p] Nothing c) (fixSize vs) cs



fixSize :: [Vertex] -> [Vertex]
fixSize vs = map (\v -> (v - m) ^* s) vs
  where m = mean vs
        s = 1 / max mx (max my mz)
        (V3 mx my mz) = maximum vs

mean :: (Fractional b, F.Foldable f) => f b -> b
mean = uncurry (/) . F.foldl' (\(s,l) v -> (s+v,l+1)) (0,0)


