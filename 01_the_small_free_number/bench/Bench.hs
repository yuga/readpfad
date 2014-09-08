import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Control.Parallel.Strategies
import Criterion.Main
import Data.Array.IO
import Data.List
import Minfree
import System.Random

main :: IO ()
main = do
    g <- getStdGen
    ns11 <- generator   (10) g
    ns12 <- generator   (10) g
    ns21 <- generator  (100) g
    ns22 <- generator  (100) g
    ns31 <- generator (1000) g
    ns32 <- generator (1000) g
    let xs11 = ns11 `using` rdeepseq
        xs12 = ns12 `using` rdeepseq
        xs21 = ns21 `using` rdeepseq
        xs22 = ns22 `using` rdeepseq
        xs31 = ns31 `using` rdeepseq
        xs32 = ns32 `using` rdeepseq
    defaultMain [

        bgroup "minfree1A" [
            bench "10^1 #1" $ nf minfree1A xs11
          , bench "10^1 #2" $ nf minfree1A xs12
          , bench "10^2 #1" $ nf minfree1A xs21
          , bench "10^2 #2" $ nf minfree1A xs22
          , bench "10^3 #1" $ nf minfree1A xs31
          , bench "10^3 #2" $ nf minfree1A xs32
          ]
      ,

        bgroup "minfree1B" [
            bench "10^1 #1" $ nf minfree1B xs11
          , bench "10^1 #2" $ nf minfree1B xs12
          , bench "10^2 #1" $ nf minfree1B xs21
          , bench "10^2 #2" $ nf minfree1B xs22
          , bench "10^3 #1" $ nf minfree1B xs31
          , bench "10^3 #2" $ nf minfree1B xs32
          ]

      , bgroup "minfree2" [
            bench "10^1 #1" $ nf minfree2 xs11
          , bench "10^1 #2" $ nf minfree2 xs12
          , bench "10^2 #1" $ nf minfree2 xs21
          , bench "10^2 #2" $ nf minfree2 xs22
          , bench "10^3 #1" $ nf minfree2 xs31
          , bench "10^3 #2" $ nf minfree2 xs32
          ]

      , bgroup "minfree3" [
            bench "10^1 #1" $ nf minfree3 xs11
          , bench "10^1 #2" $ nf minfree3 xs12
          , bench "10^2 #1" $ nf minfree3 xs21
          , bench "10^2 #2" $ nf minfree3 xs22
          , bench "10^3 #1" $ nf minfree3 xs31
          , bench "10^3 #2" $ nf minfree3 xs32
          ]
      ]

generator :: Int -> StdGen -> IO [Int]
generator n g = do
    shuffle $ fst $ runState go g
  where
    go :: State StdGen [Int] 
    go = do 
        dn <- getOne (0, min 10 n)
        ds <- getList dn
        return [x | x <- [0..n], not $ x `elem` ds]
        
shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- mkArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
  where
    n = length xs

mkArray :: Int -> [a] -> IO (IOArray Int a)
mkArray n xs = newListArray (1,n) xs

getList :: (Random a) => Int -> State StdGen [a]
getList n = do
    g <- get
    return $ (take n . unfoldr (Just . random)) g

{-         
getAny :: (Random a) => State StdGen a
getAny = do
    g <- get
    (x,g') <- return $ random g
    put g'
    return x
-}

getOne :: (Random a) => (a,a) -> State StdGen a
getOne bounds = do
    g <- get
    (x,g') <- return $ randomR bounds g
    put g'
    return x

