
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.OFF where

import Data.Attoparsec.Text

import Data.Text (Text)
import qualified Data.Text.IO as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Control.Monad
import Control.Lens
import Control.Applicative

import Linear
import Vis

off :: Parser Text 
off   = "OFF"

cnoff :: Parser Text
cnoff = "CNOFF"

--------------------------------------------------

data Header = Header {
  _vertices :: Int,
  _faces    :: Int,
  _edges    :: Int
} deriving (Show, Eq)

makeLenses ''Header

header :: Parser Header
header = Header <$> decimal <*> " " .*> decimal <*> " " .*> decimal

--------------------------------------------------

type Vertex = V3 Double
type Normal = V3 Double

v3 :: Parser (V3 Double)
v3 = V3 <$> double <*> " " .*> double <*> " " .*> double

vertex :: Parser Vertex
vertex = v3

normal :: Parser Normal
normal = v3

color :: Parser Color
color = makeColor8 <$> decimal <*> " " .*> decimal <*> " " .*> decimal <*> pure 255

--------------------------------------------------

cnoffLine :: Parser (Vertex, Normal, Color)
cnoffLine = (,,) <$> vertex <*> " " .*> normal <*> " " .*> color

--------------------------------------------------

offFormat :: Parser Text -> Parser a -> Parser (Vector a)
offFormat magic line = do
  
  -- parse and therefor check magic string
  void magic <* endOfLine

  -- read in header
  (Header v _ _) <- header <* endOfLine

  -- read in vertices
  V.replicateM v $ line <* endOfLine

parseCNOFF :: Parser (Vector (Vertex, Normal, Color))
parseCNOFF = offFormat cnoff cnoffLine

readCNOFF :: FilePath -> IO (Either String (Vector (Vertex, Normal, Color)))
readCNOFF t = parseOnly parseCNOFF <$> T.readFile t

--------------------------------------------------

