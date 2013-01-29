module MscSpec where

import Msc
import Test.Hspec
import Test.Hspec.QuickCheck         (prop)
import Test.QuickCheck        hiding ((.&.))

newtype MscTarget = MscTarget [Int] deriving (Eq, Show)

instance Arbitrary MscTarget where
    arbitrary = do
        n <- choose (0, 50)
        xs <- vectorOf n (choose (0, 10000))
        return $ MscTarget xs

spec :: Spec
spec = do
    describe "msc2" $ do
        prop "equals to msc1" $ \(MscTarget a) ->
            msc2 a == msc1 a

    describe "msc3" $ do
        prop "equals to msc1" $ \(MscTarget a) ->
            msc3 a == msc1 a

