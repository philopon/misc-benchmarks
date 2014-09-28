{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad

import qualified Control.Monad.Reader        as M
import qualified Control.Monad.State.Strict  as M
import qualified Control.Monad.Writer.Strict as M

import qualified Control.Eff as EE
import qualified Control.Eff.Lift as EE
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

runMonadIO :: Int -> IO Int
runMonadIO i = fmap (getSum . snd) . M.runWriterT $ M.runStateT (M.runReaderT (replicateM_ i monad) (1 :: Int)) (1 :: Int)

extensibleEffects :: (EE.Member (EE.Reader Int) r, EE.Member (EE.State Int) r, EE.Member (EE.Writer (Sum Int)) r) => EE.Eff r ()
extensibleEffects = do
    (d :: Int) <- EE.ask
    EE.modify (+d)
    d' <- EE.get
    EE.tell (Sum d' :: Sum Int)

runEE :: Int -> Int
runEE i = getSum . fst . EE.run $ EE.runMonoidWriter $
    EE.runState (1 :: Int) (EE.runReader (replicateM_ i extensibleEffects) (1 :: Int))

runEEIO :: Int -> IO Int
runEEIO i = fmap (getSum . fst) . EE.runLift $ EE.runMonoidWriter $
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

runEffinIO :: Int -> IO Int
runEffinIO i = fmap (getSum . snd) . Effin.runLift . Effin.runWriter $
    Effin.runState (1 :: Int) (Effin.runReader (1 :: Int) (replicateM_ i effin))

m :: Int
m = 1000

n :: Int
n = 10000

main :: IO ()
main = defaultMain
    [ bgroup "pure" 
        [ bgroup "monad"
            [ bench (show m) $ nf runMonad m
            , bench (show n) $ nf runMonad n
            ]
        , bgroup "extensible-effects"
            [ bench (show m) $ nf runEE m
            , bench (show n) $ nf runEE n
            ]
        , bgroup "effin"
            [ bench (show m) $ nf runEffin m
            , bench (show n) $ nf runEffin n
            ]
        ]
    , bgroup "IO"
        [ bgroup "monad"
            [ bench (show m) $ nfIO $ runMonadIO m
            , bench (show n) $ nfIO $ runMonadIO n
            ]
        , bgroup "extensible-effects"
            [ bench (show m) $ nfIO $ runEEIO m
            , bench (show n) $ nfIO $ runEEIO n
            ]
        , bgroup "effin"
            [ bench (show m) $ nfIO $ runEffinIO m
            , bench (show n) $ nfIO $ runEffinIO n
            ]
        ]
    ]
