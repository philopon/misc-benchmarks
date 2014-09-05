import Control.Exception
import Criterion.Main
import Foreign.Marshal.Alloc

benchAlloca, benchMalloc :: Int -> IO ()
benchAlloca size = allocaBytes size $ \_ -> return ()
benchMalloc size = bracket (mallocBytes size) free $ \_ -> return ()

benchmark :: Int -> Benchmark
benchmark size = bgroup (show size)
    [ bench "alloca" . nfIO $ benchAlloca size
    , bench "malloc" . nfIO $ benchMalloc size
    ]

main :: IO ()
main = defaultMain
    [ benchmark 8
    , benchmark 128
    , benchmark 2048
    , benchmark 32768
    , benchmark 131072
    ]
