{-# LANGUAGE OverloadedStrings #-}

-- random sources for data

module Data.Random where

import qualified Control.Foldl as L
import           Control.Monad
import qualified Data.Vector.Unboxed as V
import           Pipes
import qualified Pipes.Prelude as Pipes
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD
import Control.Monad.Primitive

mwc0 :: (PrimMonad m) => m (MWC.Gen (PrimState m))
mwc0 = MWC.initialize $ V.fromList [42]
{-# INLINE mwc0 #-}

-- random variate sources
rv :: Int -> IO [Double]
rv n = do
    mwc <- mwc0
    replicateM n (MWCD.standard mwc)
{-# INLINE rv #-}

-- correlated pairs
rvcorr :: Int -> Double -> IO [(Double,Double)]
rvcorr n c = do
    mwc <- mwc0
    a0 <- replicateM n (MWCD.standard mwc)
    a1 <- replicateM n (MWCD.standard mwc)
    let a2 = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) a0 a1
    return $ Prelude.zip a0 a2
{-# INLINE rvcorr #-}

-- random normal producer
rvP :: Double -> Double -> Producer Double IO ()
rvP mean vol = do
    mwc <- lift mwc0
    forever $ do
        a <- lift $ MWCD.standard mwc
        yield $ mean + vol * a
{-# INLINE rvP #-}

-- correlated pair producer
rvcorrP :: Double -> Producer (Double,Double) IO ()
rvcorrP c = do
    mwc <- lift mwc0
    forever $ do
        a0 <- lift $ MWCD.standard mwc
        a1 <- lift $ MWCD.standard mwc
        let a2 = (\x y -> c * x + sqrt (1 - c * c) * y) a0 a1
        yield (a0, a2)
{-# INLINE rvcorrP #-}

-- random walk
walkP :: Double -> Double -> Double -> Producer Double IO ()
walkP start mean vol = rvP mean vol >-> L.purely Pipes.scan (L.Fold (+) start id)
{-# INLINE walkP #-}

