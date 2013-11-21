
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import qualified Data.Vector as V

import Control.Applicative

import qualified Data.KDTree as KD

import Data.OFF 

import Data.List
import Data.Function

import Linear

import Data.Word

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (V.Vector Vertex) where
  arbitrary = V.fromList . getNonEmpty <$> arbitrary 


prop_hkd_is_hls :: (Int,Vertex,V.Vector Vertex) -> Bool
prop_hkd_is_hls (d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbor (KD.kdtree 8 . V.convert $ vs) p
        linSearch  = Just $ V.minimumBy (compare `on` qd p) vs

prop_kd_is_ls :: (Int,Vertex,V.Vector Vertex) -> Bool
prop_kd_is_ls (d,p,vs) = treeSearch == linSearch
  where treeSearch = KD.nearestNeighbors (KD.kdtree 8 . V.convert $ vs) p
        linSearch  = sortBy (compare `on` qd p) . V.toList $ vs

main :: IO ()
main = $defaultMainGenerator
