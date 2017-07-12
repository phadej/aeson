{-# LANGUAGE PackageImports #-}
module Typed.Manual (benchmarks, decodeBenchmarks, validate) where

import Prelude ()
import Prelude.Compat

import "aeson" Data.Aeson hiding (Result)
import Criterion
import Data.ByteString.Lazy as L
import Twitter.Manual
import Typed.Common
import qualified "aeson-benchmarks" Data.Aeson as B
import qualified "aeson-benchmarks" Data.Aeson.Stream as B
import qualified "aeson-benchmarks" Data.Aeson.Stream.Parser as B

encodeDirectA :: Result -> L.ByteString
encodeDirectA = encode

encodeViaValueA :: Result -> L.ByteString
encodeViaValueA = encode . toJSON

encodeDirectB :: Result -> L.ByteString
encodeDirectB = B.encode

encodeViaValueB :: Result -> L.ByteString
encodeViaValueB = B.encode . B.toJSON

benchmarks :: Benchmark
benchmarks =
  env ((,) <$> load "json-data/twitter100.json" <*> load "json-data/jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "encodeManual" [
      bgroup "direct" [
        bench "twitter100"          $ nf encodeDirectB twitter100
      , bench "jp100"               $ nf encodeDirectB jp100
      , bench "twitter100 baseline" $ nf encodeDirectA twitter100
      , bench "jp100 baseline"      $ nf encodeDirectA jp100
      ]
    , bgroup "viaValue" [
        bench "twitter100"          $ nf encodeViaValueB twitter100
      , bench "jp100"               $ nf encodeViaValueB jp100
      , bench "twitter100 baseline" $ nf encodeViaValueA twitter100
      , bench "jp100 baseline"      $ nf encodeViaValueA jp100
      ]
    ]

decodeDirectA :: L.ByteString -> Maybe Result
decodeDirectA = decode

decodeDirectB :: L.ByteString -> Maybe Result
decodeDirectB = B.decode

decodeTokenParser :: L.ByteString -> Either String Result
decodeTokenParser = B.decodeStream . B.tokenStream

decodeBenchmarks :: Benchmark
decodeBenchmarks =
  env ((,) <$> L.readFile "json-data/twitter100.json" <*> L.readFile "json-data/jp100.json") $ \ ~(twitter100, jp100) ->
  bgroup "decodeManual"
    [ bgroup "direct"
      [ bench "twitter100"          $ nf decodeDirectB twitter100
      , bench "jp100"               $ nf decodeDirectB jp100
      , bench "twitter100 baseline" $ nf decodeDirectA twitter100
      , bench "jp100 baseline"      $ nf decodeDirectA jp100
      ]
    , bgroup "TokenParser"
      [ bench "twitter100"          $ nf decodeTokenParser twitter100
      ]
    ]

validate :: IO ()
validate = do
  twitter100 <- L.readFile "json-data/twitter100.json"
  d <- maybe (fail "direct") pure $ decodeDirectB twitter100
  t <- either fail pure $ decodeTokenParser twitter100
  print (d == t)
