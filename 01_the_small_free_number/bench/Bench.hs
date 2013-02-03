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
    ns1 <- generator (100) g
    ns2 <- generator (100) g
    let xs1 = ns1 `using` rdeepseq
        xs2 = ns2 `using` rdeepseq
    defaultMain [
        bgroup "minfree1A" [
            bench "10^2 #1" $ nf minfree1A xs1
          , bench "10^2 #2" $ nf minfree1A xs2
          ]

      , bgroup "minfree1B" [
            bench "10^2 #1" $ nf minfree1B xs1
          , bench "10^2 #2" $ nf minfree1B xs2
          ]
      , bgroup "minfree2" [
            bench "10^2 #1" $ nf minfree2 xs1
          , bench "10^2 #2" $ nf minfree2 xs2
          ]
      , bgroup "minfree3" [
            bench "10^2 #1" $ nf minfree3 xs1
          , bench "10^2 #2" $ nf minfree3 xs2
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

