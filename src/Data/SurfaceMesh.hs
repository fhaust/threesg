{-# LANGUAGE TemplateHaskell #-}

module Data.SurfaceMesh where


import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Foldable as F

import Data.KDTree (KDTree, kdtreeBy)

import Linear

import Control.Applicative
import Control.Lens
import Control.DeepSeq

import Data.Instances

type Position = V3 Double
type Normal   = V3 Double

type VID    = Int


data Vertex = Vertex
  { _position :: Position
  , _normal   :: Normal
  } deriving (Show, Eq)

makeLenses ''Vertex

data SurfaceMesh = SurfaceMesh 
  { _positions :: V.Vector Position
  , _normals   :: V.Vector Normal
  , _kdtree    :: KDTree UV.Vector VID
  }

makeLenses ''SurfaceMesh

surfaceMesh vs ns = SurfaceMesh vs ns kd
  where kd = kdtreeBy (vs V.!) 8 is
        is = V.enumFromN 0 (V.length vs)


surfaceMesh' vs = SurfaceMesh ps ns kd
  where ps = V.map _position vs
        ns = V.map _normal   vs
        kd = recalcKD ps

vertices :: Lens' SurfaceMesh (V.Vector Vertex)
vertices f (SurfaceMesh ps ns kd) = fmap fun1 (f $ V.zipWith Vertex ps ns)
  where fun1 vs' = SurfaceMesh ps' ns' kd' 
          where ps' = V.map _position vs'
                ns' = V.map _normal   vs'
                kd' = recalcKD ps' 

--traverseVertices :: Traversal' SurfaceMesh Vertex
--traverseVertices :: (Applicative f) => (Vertex -> f Vertex) -> SurfaceMesh -> f SurfaceMesh
--traverseVertices f sm = SurfaceMesh <$> ps' <*> ns' <*> kd'
--  where vs  = V.zipWith Vertex (sm^.positions) (sm^.normals)
--        vs' = V.map f vs
--        ps' = V.map (fmap _position) vs'
--        ns' = V.map (fmap _normal)   vs'
--        kd' = recalcKD ps'

--------------------------------------------------

bounds :: SurfaceMesh -> (V3 Double, V3 Double)
bounds sm = (minV, maxV)
  where minV = F.foldl' (liftU2 min) 99999999999 (_positions sm)
        maxV = F.foldl' (liftU2 max) (-999999999)(_positions sm)

--------------------------------------------------

recalcKD :: V.Vector Position -> KDTree UV.Vector Int
recalcKD vs = kdtreeBy (vs V.!) 8 (V.enumFromN 0 (V.length vs))

--------------------------------------------------

instance NFData SurfaceMesh where
  rnf (SurfaceMesh ps ns kd) = rnf ps `seq` rnf ns `seq` rnf kd `seq` ()
