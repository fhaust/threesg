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
kdtree d fs | d < 1 || V.null fs = Leaf fs
            | otherwise = do
              
              let p = mean fs
              --let n = normalize . stddev p $ fs
              let n = [V3 1 0 0, V3 0 1 0, V3 0 0 1] !! (d `mod` 3)

              let (l,r) = V.partition (\x -> distPlanePoint p n x < 0) fs

              Node p n (kdtree (d-1) l) (kdtree (d-1) r)




--------------------------------------------------

--nearestNeighbor t q = head $ nearestNeighbors t q

nearestNeighbor (Leaf vs)        q | V.null vs = Nothing
                                   | otherwise = Just $ V.minimumBy (compare `on` qd q) vs
nearestNeighbor (Node p n l r) q =

  --let d = dist p n q
  let d = distPlanePoint p n q 

      nnl = nearestNeighbor l q
      nnr = nearestNeighbor r q

  in if d < 0 then decide q d nnl nnr 
              else decide q d nnr nnl

{-# INLINE decide #-}
decide _ _ Nothing  b         = b
decide q d (Just a) (Just b)  | qd q a < d^(2::Int) || (qd q a < qd q b) = Just a
                              | otherwise                                = Just b
decide _ _ a        Nothing   = a
--------------------------------------------------

nearestNeighbors (Leaf vs)      q = L.sortBy (compare `on` qd q) 
                                  . V.toList $ vs
nearestNeighbors (Node p n l r) q =

  let d   = distPlanePoint p n q 
   
      nnl = nearestNeighbors l q 
      nnr = nearestNeighbors r q 

  in if d < 0 then ordMerge q p n d nnl nnr
              else ordMerge q p n d nnr nnl
  
{-# INLINE ordMerge #-}
--ordMerge _ _ _ _ []     bs      = bs
--ordMerge q p n d (a:as) bs      | qd q a < d^(2::Int) = a : ordMerge q p n d as bs 
--ordMerge _ _ _ _ as     []      = as
--ordMerge q p n d (a:as) (b:bs)  | qd q a < qd q b     = a : ordMerge q p n d as (b:bs) 
--                                | otherwise           = b : ordMerge q p n d (a:as) bs

ordMerge q p n d = go
  where go []     bs     = bs
        go (a:as) bs     | qdq a < d^(2::Int) = a : go as bs
        go as     []     = as
        go (a:as) (b:bs) | qdq a < qdq b = a : go as (b:bs)
                         | otherwise     = b : go (a:as) bs

        qdq = qd q



--ordMerge q p n d as bs = safe ++ Ord.mergeBy (compare `on` qd q) unsafe bs
--  where (safe,unsafe) = V.span (\x -> qd q x < d^(2::Int)) as


             


--------------------------------------------------

{-# INLINE distPlanePoint #-}
distPlanePoint :: (Metric f, Num a) => f a -> f a -> f a -> a 
distPlanePoint p n x = n `dot` (x ^-^ p)

{-# INLINE dist' #-}
dist' :: (a -> Double) -> a -> a -> Double
dist' f a b = f a - f b

mean :: (Fractional c, V.Storable c) => V.Vector c -> c
mean = uncurry (/) . V.foldl' (\(s,l) b -> (s+b,l+1)) (0,0)

--------------------------------------------------

instance NFData a => NFData (KDTree a) where
  rnf (Leaf vs)      = rnf vs
  rnf (Node p n l r) = rnf p `seq` rnf n `seq` rnf l `seq` rnf r `seq` ()
