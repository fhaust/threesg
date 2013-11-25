{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BangPatterns #-}

module Data.KDTree where


import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Storable as V
import qualified Data.List            as L

import Data.Function

import Linear

import Control.DeepSeq

type Distance = Double

data KDTree a = Node { _point  :: !(V3 Double)
                     , _normal :: !(V3 Double)
                     , _left   :: KDTree a
                     , _right  :: KDTree a
                     }
              | Leaf { _bucket :: V.Vector a }

  deriving (Show, Read, Eq)              

--------------------------------------------------

kdtree :: Int -> V.Vector (V3 Double) -> KDTree (V3 Double)
kdtree = kdtreeBy id

kdtreeBy :: (V.Storable a) => (a -> V3 Double) -> Int -> V.Vector a -> KDTree a 
kdtreeBy f d fs | d < 1 || V.length fs < 64 = Leaf fs
                | otherwise = do
                  
                  let p = mean . V.map f $ fs 


                  --let n = normalize . stddev p $ fs
                  let n = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)

                  let (l,r) = V.partition (\x -> distPlanePoint p n (f x) < 0) fs

                  Node p n (kdtreeBy f (d-1) l) (kdtreeBy f (d-1) r)





--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors :: KDTree (V3 Double) -> V3 Double -> [V3 Double]
nearestNeighbors = nearestNeighborsBy id

nearestNeighborsBy :: (V.Storable a) => (a -> V3 Double) -> KDTree a -> V3 Double -> [a]
nearestNeighborsBy f (Leaf vs)      q = L.sortBy (compare `on` (qd q . f)) . V.toList $ vs
nearestNeighborsBy f (Node p n l r) q = if d < 0 then go nnl nnr else go nnr nnl

  where d   = distPlanePoint p n q

        nnl = nearestNeighborsBy f l q 
        nnr = nearestNeighborsBy f r q 

        -- recursively merge the two children
        -- the second line makes sure that points in the
        -- 'safe' region are prefered 
        go []     bs     = bs
        go (a:as) bs     | qdq a < (d*d) = a : go as bs
        go as     []     = as
        go (a:as) (b:bs) | qdq a < qdq b      = a : go as (b:bs)
                         | otherwise          = b : go (a:as) bs

        -- quadratic distance to query point
        qdq = qd q . f

--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree
nearestNeighbor :: KDTree (V3 Double) -> V3 Double -> V3 Double
nearestNeighbor = nearestNeighborBy id

nearestNeighborBy :: (V.Storable a) => (a -> V3 Double) -> KDTree a -> V3 Double -> a
nearestNeighborBy f t = head . nearestNeighborsBy f t 

--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround :: KDTree (V3 Double) -> Double -> V3 Double -> [V3 Double]
pointsAround = pointsAroundBy id

pointsAroundBy :: (V.Storable a) => (a -> V3 Double) -> KDTree a -> Double -> V3 Double -> [a]
pointsAroundBy f t r q = takeWhile (\p -> qd q (f p) < (r*r)) . nearestNeighborsBy f t $ q

--------------------------------------------------

{-# INLINE distPlanePoint #-}
distPlanePoint :: (Metric f, Num a) => f a -> f a -> f a -> a 
distPlanePoint p n x = n `dot` (x ^-^ p)

mean = uncurry (/) . V.foldl' (\(!s,!l) b -> (s+b,l+1)) (0,0)

stddev m vs = fmap sqrt . (/ (n-1)) . V.sum . V.map (\x -> (x - m)^2) $ vs
  where n = fromIntegral . V.length $ vs
        

--------------------------------------------------

instance NFData a => NFData (KDTree a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node _ _ l r) = rnf l `seq` rnf r `seq` ()
