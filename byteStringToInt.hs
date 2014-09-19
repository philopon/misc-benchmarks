{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lex.Integral as L

doLex :: (S8.ByteString -> Maybe (Int, S8.ByteString)) -> S8.ByteString -> Int
doLex p s = case p s of
    Nothing -> error "no read"
    Just (a, o) | S8.null o -> a
                | otherwise -> error "no read"

asInt :: Int -> Int
asInt = id

main :: IO ()
main = defaultMain
    [ bgroup "Data.ByteString.Char8.readInt"
        [ bench  "12365563412546" $ nf S8.readInt  "12365563412546"
        , bench "-12365563412546" $ nf S8.readInt "-12365563412546"
        ]
    , bgroup "Data.ByteString.Lex.Integral(signed)"
        [ bench  "12365563412546"  $ nf (doLex $ L.readSigned L.readDecimal) "12365563412546" 
        , bench  "-12365563412546" $ nf (doLex $ L.readSigned L.readDecimal) "-12365563412546" 
        ]
    , bgroup "Data.ByteString.Lex.Integral(unsigned)"
        [ bench  "12365563412546"  $ nf (doLex L.readDecimal) "12365563412546" 
        ]
    , bgroup "read . unpack"
        [ bench  "12365563412546"  $ nf (asInt . read . S8.unpack) "12365563412546" 
        , bench "-12365563412546"  $ nf (asInt . read . S8.unpack) "-12365563412546" 
        ]
    ]
