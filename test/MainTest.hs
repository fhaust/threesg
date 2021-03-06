
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector as V

import Control.Applicative

import qualified Data.KDTree as KD
import qualified Data.LinSearch as LS

import Data.OFF 

import Data.List
import Data.Function

import Linear

import Data.Word

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (V.Vector Vertex) where
  arbitrary = V.fromList . getNonEmpty <$> arbitrary 


prop_nn :: (Int,Vertex,V.Vector Vertex) -> Bool
prop_nn (d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor (KD.kdtree d . V.convert $ vs) p
        linSearch  = LS.nearestNeighbor vs p

prop_nns :: (Int,Vertex,V.Vector Vertex) -> Bool
prop_nns (d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors (KD.kdtree d . V.convert $ vs) p
        linSearch  = LS.nearestNeighbors vs p 

prop_nr :: (Int,Vertex,Double,V.Vector Vertex) -> Bool
prop_nr (d,p,r,vs) = treeSearch == linSearch
  where treeSearch = KD.pointsAround (KD.kdtree d . V.convert $ vs) r p
        linSearch  = LS.pointsAround vs r p

main :: IO ()
main = $defaultMainGenerator
