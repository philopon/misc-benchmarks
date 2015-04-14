{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Control.Monad.Primitive(PrimMonad, PrimState)

import Data.Word(Word64)

import qualified System.Random     as Random
import qualified System.Random.MWC as MWC
import qualified System.Random.Mersenne.Pure64 as MTPure
import qualified System.Random.Mersenne as MT
import qualified System.Random.SFMT as SFMT
import qualified System.Random.TF as TF
import qualified Random.Xorshift.Int32 as X32
import qualified Random.Xorshift.Int64 as X64
import qualified System.Random.Xorshift128Plus as X128

n :: Int
n = 1000

sumRandomGen :: Random.RandomGen gen => Int -> gen -> Word
sumRandomGen = loop 0
  where
    loop !tot !i gen
        | i <= 0    = tot
        | otherwise = let (a, gen') = Random.random gen
                      in loop (tot + a `quot` 1000) (i - 1) gen'

sumMTRandomGen :: Int -> MT.MTGen -> IO Word
sumMTRandomGen lim gen = loop lim 0
  where
    loop !i !tot
        | i <= 0    = return tot
        | otherwise = do
            a <- MT.random gen
            loop (i - 1) (tot + a `quot` 1000)

sumMWCGen :: PrimMonad m =>Int -> MWC.Gen (PrimState m) -> m Word
sumMWCGen lim gen = loop lim 0
  where
    loop !i !tot
        | i <= 0    = return tot
        | otherwise = do
            a <- MWC.uniform gen
            loop (i - 1) (tot + a `quot` 1000)

sumX128Gen :: Int -> X128.Gen -> Word64
sumX128Gen lim gen = loop lim 0 gen
  where
    loop !i !tot g
        | i <= 0    = tot
        | otherwise = do
            let (a, g') = X128.next g
            loop (i - 1) (tot + a `quot` 1000) g'

sumSFMTGen :: PrimMonad m =>Int -> SFMT.Gen (PrimState m) -> m Word
sumSFMTGen lim gen = loop lim 0
  where
    loop !i !tot
        | i <= 0    = return tot
        | otherwise = do
            a <- SFMT.uniform gen
            loop (i - 1) (tot + a `quot` 1000)

baseLine :: Int -> Word
baseLine lim = loop lim 0
  where
    loop !i !tot
        | i <= 0    = tot
        | otherwise = loop (i - 1) (tot + fromIntegral i `quot` 1000)

main :: IO ()
main = do
    !randomGen <- Random.getStdGen
    !mtpGen    <- MTPure.newPureMT
    !mtGen     <- MT.getStdGen
    !mwcGen    <- MWC.createSystemRandom
    !sfmtGen   <- SFMT.createSystemRandom
    !tfGen     <- TF.newTFGen
    !x32Gen    <- X32.newXorshift32
    !x64Gen    <- X64.newXorshift64
    let !x128Gen   = X128.initialize 84782837492
    defaultMain
        [ bench "L'Ecuyer" $ nf (sumRandomGen n) randomGen
        , bgroup "MT"
            [ bench "Pure64" $ nf (sumRandomGen   n) mtpGen
            , bench "IO"     $ nfIO (sumMTRandomGen n mtGen)
            , bench "SFMT"   $ nfIO (sumSFMTGen n sfmtGen)
            ]
        , bench "MWC" $ nfIO (sumMWCGen n mwcGen)
        , bench "tf"  $ nf (sumRandomGen n) tfGen
        , bgroup "xorshift"
            [ bench "32bit" $ nf (sumRandomGen n) x32Gen
            , bench "64bit" $ nf (sumRandomGen n) x64Gen
            ]
        , bench "xorshift+128" $ nf (sumX128Gen n) x128Gen
        , bench "baseline" $ nf baseLine n
        ]
