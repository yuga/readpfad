module MinfreeSpec where

import Data.List
import Minfree
import Test.Hspec
import Test.Hspec.QuickCheck         (prop)
import Test.QuickCheck        hiding ((.&.))

newtype MinfreeTarget = MinfreeTarget [Int] deriving (Eq, Show)

instance Arbitrary MinfreeTarget where
    arbitrary = do
        n <- choose (0, 1000)
        dn <- choose (0, min 10 n)
	ms <- return $ if n == 0 then [] else [0..(n-1)]
        ds <- vectorOf dn (choose (0, (n-1)))
        xs <- shuffle [x | x <- ms, not $ x `elem` ds]
        return $ MinfreeTarget xs

shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do
    x <- oneof $ map return xs
    ys <- shuffle $ delete x xs
    return (x:ys)

propt :: Testable prop => String -> prop -> Spec
propt s = it s . (printTestCase s)

spec :: Spec
spec = do
    describe "minfree1B" $ do
        propt "equals to minfree1A" $ \(MinfreeTarget xs) ->
            minfree1B xs == minfree1A xs

    describe "minfree2" $ do
        prop "equals to minfree1A" $ \(MinfreeTarget xs) ->
            minfree2  xs == minfree1A xs
    
    describe "minfree3" $ do
        prop "equals to minfree1A" $ \(MinfreeTarget xs) ->
            minfree3  xs == minfree1A xs
