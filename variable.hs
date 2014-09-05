import Control.Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.IORef as IORef
import qualified Control.Concurrent.MVar as MVar
import Criterion.Main

count :: Int
count = 100000

state :: Int -> Int
state = State.execState (replicateM_ count $ State.modify succ)

ioRef :: IO Int
ioRef = do
    ref <- IORef.newIORef (0 :: Int)
    replicateM_ count $ IORef.modifyIORef' ref succ
    IORef.readIORef ref

atomicIORef :: IO Int
atomicIORef = do
    ref <- IORef.newIORef (0 :: Int)
    replicateM_ count $ IORef.atomicModifyIORef' ref (\i -> (succ i, ()))
    IORef.readIORef ref

mVar :: IO Int
mVar = do
    mvar <- MVar.newMVar (0 :: Int)
    replicateM_ count $ MVar.modifyMVar_ mvar (\i -> return $! succ i)
    MVar.readMVar mvar

main :: IO ()
main = defaultMain
    [ bench "State"         $ nf state 0
    , bench "IORef"         $ nfIO ioRef
    , bench "IORef(atomic)" $ nfIO atomicIORef
    , bench "mvar"          $ nfIO mVar
    ]
