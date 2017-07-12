module Main (main) where

import Prelude ()
import Prelude.Compat

import Criterion.Main
import qualified Typed.Generic as Generic
import qualified Typed.Manual as Manual
import qualified Typed.TH as TH

main :: IO ()
main = Manual.validate >> defaultMain [
    Generic.benchmarks
  , Manual.benchmarks
  , TH.benchmarks
  , Generic.decodeBenchmarks
  , Manual.decodeBenchmarks
  , TH.decodeBenchmarks
  ]
