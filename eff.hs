{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad

import qualified Control.Monad.Reader        as M
import qualified Control.Monad.State.Strict  as M
import qualified Control.Monad.Writer.Strict as M

import qualified Control.Eff as EE
import qualified Control.Eff.State.Strict  as EE
import qualified Control.Eff.Reader.Strict as EE
import qualified Control.Eff.Writer.Strict as EE

import qualified Control.Effect as Effin

import Data.Monoid
import Data.Typeable
import Criterion.Main

deriving instance Typeable Sum

monad :: (M.MonadReader Int m, M.MonadWriter (Sum Int) m, M.MonadState Int m) => m ()
monad = do
    d  <- M.ask
    M.modify (+d)
    d' <- M.get
    M.tell (Sum d')

runMonad :: Int -> Int
runMonad i = getSum . snd . M.runWriter $ M.runStateT (M.runReaderT (replicateM_ i monad) (1 :: Int)) (1 :: Int)

extensibleEffects :: (EE.Member (EE.Reader Int) r, EE.Member (EE.State Int) r, EE.Member (EE.Writer (Sum Int)) r) => EE.Eff r ()
extensibleEffects = do
    (d :: Int) <- EE.ask
    EE.modify (+d)
    d' <- EE.get
    EE.tell (Sum d' :: Sum Int)

runEE :: Int -> Int
runEE i = getSum . fst . EE.run $ EE.runMonoidWriter $
    EE.runState (1 :: Int) (EE.runReader (replicateM_ i extensibleEffects) (1 :: Int))

effin :: (Effin.EffectWriter (Sum Int) l, Effin.EffectReader Int l, Effin.EffectState Int l) => Effin.Effect l ()
effin = do
    (d :: Int) <- Effin.ask
    Effin.modify (+d)
    d' <- Effin.get
    Effin.tell (Sum d' :: Sum Int)

runEffin :: Int -> Int
runEffin i = getSum . snd . Effin.runEffect . Effin.runWriter $
    Effin.runState (1 :: Int) (Effin.runReader (1 :: Int) (replicateM_ i effin))

main :: IO ()
main = defaultMain
    [ bench "monad"              $ nf runMonad 100000
    , bench "extensible-effects" $ nf runEE    100000
    , bench "effin"              $ nf runEffin 100000
    ]
