module InvertSpec where

import Data.List
import Invert
import Test.Hspec
import Test.Hspec.QuickCheck         (prop)
import Test.QuickCheck        hiding ((.&.))

f0,f1,f2,f3,f4,fx :: (Integer,Integer) -> Integer
f0 (x,y) = 2^y * (2*x + 1) - 1
f1 (x,y) = x*2^y + y*2^y + 2*x + y
f2 (x,y) = 3*x + 27*y + y^(2::Integer)
f3 (x,y) = x^(2::Integer) + y^(2::Integer) + x + y
f4 (x,y) = x + 2^y + y - 1
fx (x,y) = 10*x + y

newtype InvertTarget1 = InvertTarget1 Integer
                     deriving (Eq,Show)

instance Arbitrary InvertTarget1 where
    arbitrary = InvertTarget1 `fmap` choose (0,12)

newtype InvertTarget2 = InvertTarget2 Integer
                     deriving (Eq,Show)

instance Arbitrary InvertTarget2 where
    arbitrary = InvertTarget2 `fmap` choose (0,200)

specTemplate :: String
             -> (((Integer,Integer) -> Integer) -> Integer -> [(Integer,Integer)])
             -> Spec
specTemplate name invert = do
    describe name $ do
        prop "equals to invert7 with f0" $ \(InvertTarget1 z) ->
            invert' f0 z == invert7' f0 z
        prop "equals to invert7 with f1" $ \(InvertTarget1 z) ->
            invert' f1 z == invert7' f1 z
        prop "equals to invert7 with f2" $ \(InvertTarget2 z) ->
            invert' f2 z == invert7' f2 z
        prop "equals to invert7 with f3" $ \(InvertTarget2 z) ->
            invert' f3 z == invert7' f3 z
        prop "equals to invert7 with f4" $ \(InvertTarget1 z) ->
            invert' f4 z == invert7' f4 z
        prop "equals to invert7 with fx" $ \(InvertTarget2 z) ->
            invert' fx z == invert7' fx z
  where
    dot2     = ((.) . (.))
    invert'  = sort `dot2` invert
    invert7' = sort `dot2` invert7

spec :: Spec
spec = do
    specTemplate "invert1" invert1
    specTemplate "invert2" invert2
    specTemplate "invert3" invert3
    specTemplate "invert4" invert4
    specTemplate "invert5" invert5
    specTemplate "invert6" invert6

