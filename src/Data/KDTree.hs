{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.KDTree where

import qualified Data.Vector.Storable as V
import qualified Data.List            as L
import qualified Data.List.Ordered    as Ord 

import Data.Function

import Linear

import Control.Lens
import Control.DeepSeq

type Distance = Double

data KDTree a = Node { _point  :: a
                     , _normal :: a
                     , _left   :: KDTree a
                     , _right  :: KDTree a
                     }
              | Leaf { _bucket :: V.Vector a }

  deriving (Show, Read, Eq)              

--------------------------------------------------

kdtree :: Int -> V.Vector (V3 Double) -> KDTree (V3 Double) 
kdtree d fs | d < 1 || V.length fs < 64 = Leaf fs
            | otherwise = do
              
              let p = mean fs


              --let n = normalize . stddev p $ fs
              let n = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)

              let (l,r) = V.partition (\x -> distPlanePoint p n x < 0) fs

              Node p n (kdtree (d-1) l) (kdtree (d-1) r)




--------------------------------------------------

-- | get the nearest neighbor of point q
-- | note: dies if you pass it an empty tree
nearestNeighbor t = head . nearestNeighbors t

--------------------------------------------------

-- | get all points in the tree, sorted by distance to the 'q'uery point
-- | this is the 'bread and butter' function and should be quite fast
nearestNeighbors (Leaf vs)      q = L.sortBy (compare `on` qd q) . V.toList $ vs
nearestNeighbors (Node p n l r) q = if d < 0 then go nnl nnr else go nnr nnl

  where d   = distPlanePoint p n q

        nnl = nearestNeighbors l q 
        nnr = nearestNeighbors r q 

        -- recursively merge the two children
        -- the second line makes sure that points in the
        -- 'safe' region are prefered 
        go []     bs     = bs
        go (a:as) bs     | qdq a < d^(2::Int) = a : go as bs
        go as     []     = as
        go (a:as) (b:bs) | qdq a < qdq b      = a : go as (b:bs)
                         | otherwise          = b : go (a:as) bs

        -- quadratic distance to query point
        qdq = qd q

--------------------------------------------------

-- | return the points around a 'q'uery point up to radius 'r'
pointsAround t r q = takeWhile ((< r) . qd q) nns
  where nns = nearestNeighbors t q

--------------------------------------------------

{-# INLINE distPlanePoint #-}
distPlanePoint :: (Metric f, Num a) => f a -> f a -> f a -> a 
distPlanePoint p n x = n `dot` (x ^-^ p)

mean = uncurry (/) . V.foldl' (\(s,l) b -> (s+b,l+1)) (0,0)

stddev m vs = fmap sqrt . (/ (n-1)) . V.sum . V.map (\x -> (x - m)^2) $ vs
  where n = fromIntegral . V.length $ vs
        

--------------------------------------------------

instance NFData a => NFData (KDTree a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node p n l r) = rnf p `seq` rnf n `seq` rnf l `seq` rnf r `seq` ()
