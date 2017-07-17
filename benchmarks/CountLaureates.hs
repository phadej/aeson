-- This benchmark is used to measure heap usage
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Stream
import Data.Aeson.Stream.Parser
import System.Environment (getArgs)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.Set (Set)
import Data.Foldable (traverse_, toList)

import qualified Data.ByteString.Lazy as BL

-- | "Envelope"
newtype Laureates a = Laureates { getLaureates :: a }
  deriving Show

newtype Count = Count { getCount :: Int }
  deriving Show

newtype Surname = Surname (Maybe Text)
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- parseJSON
-------------------------------------------------------------------------------

countParseJSON :: BL.ByteString -> Either String (Laureates Count)
countParseJSON = eitherDecode

surnamesParseJSON :: BL.ByteString -> Either String (Laureates (Set Surname))
surnamesParseJSON = eitherDecode

instance FromJSON a => FromJSON (Laureates a) where
    parseJSON = withObject "Laureates" $ \obj -> Laureates
        <$> obj .: "laureates"

instance FromJSON Count where
    parseJSON = withArray "Count" $ \v ->
        pure $ Count $ length (v :: Vector Value)

instance FromJSON Surname where
    parseJSON = withObject "Laureate" $ \obj -> Surname
        <$> obj .:? "surname"

-------------------------------------------------------------------------------
-- tokenStream
-------------------------------------------------------------------------------

-- | A bit manual ATM
countTokenStream :: BL.ByteString -> Either String (Laureates Count)
countTokenStream s = case tokenStream s of
    TkObjectOpen : TkKey "laureates" :  TkArrayOpen : rest -> go 0 rest
    _ -> Left "invalid input"
  where
    go !acc [TkArrayClose, TkObjectClose] = Right (Laureates (Count acc))
    go !acc ts = do
        ts' <- skip ts
        go (acc + 1) ts'

-- We count brackets, but cheat a bit
skip :: TokenStream -> Either String TokenStream
skip = go (0 :: Int)
  where
    go _ [] = Left "Unexpected end-of-input"
    go !acc (t : ts) = case t of
        TkNull        -> done acc ts
        TkTrue        -> done acc ts
        TkFalse       -> done acc ts
        TkText _      -> done acc ts
        TkNumber _    -> done acc ts
        TkKey _       -> go acc ts
        TkObjectOpen  -> go (acc + 1) ts
        TkArrayOpen   -> go (acc + 1) ts
        TkObjectClose -> done (acc - 1) ts
        TkArrayClose  -> done (acc - 1) ts
        TkError err   -> Left err

    done !n ts | n <= 0    = Right ts
               | otherwise = go n ts

-------------------------------------------------------------------------------
-- TokenParser
-------------------------------------------------------------------------------

countTokenParser :: BL.ByteString -> Either String (Laureates Count)
countTokenParser = decodeStream . tokenStream

surnamesTokenParser :: BL.ByteString -> Either String (Laureates (Set Surname))
surnamesTokenParser = decodeStream . tokenStream

instance FromStream a => FromStream (Laureates a) where
    parseStream = withObjectP "Laureates" $ Laureates
        <$> objectField "laureates"

instance FromStream Count where
    parseStream = Count <$> arrayLength

instance FromStream Surname where
    parseStream = withObjectP "Laureate" $ Surname
        <$> objectFieldMaybe "surname"

arrayLength :: P s Int
arrayLength = foldArray' 0 (\n _ -> n + 1) skipValue

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    contents <- BL.readFile "json-data/laureate.json"
    counts contents args

counts :: BL.ByteString -> [String] -> IO ()
counts contents args = case args of
    ["parseJSON"]   -> k countParseJSON
    ["tokenStream"] -> k countTokenStream
    ["TokenParser"] -> k countTokenParser
    _ -> surnames contents args
  where
    k counter = either fail (print . getCount . getLaureates) $ counter contents

surnames :: BL.ByteString -> [String] -> IO ()
surnames contents args = case args of
    ["parseJSON-surnames"]   -> k surnamesParseJSON
    ["TokenParser-surnames"] -> k surnamesTokenParser
    _ -> fail "specify variant: parseJSON, tokenStream, TokenParser"
  where
    k p = either fail (traverse_ print . take 10 . toList . getLaureates) $ p contents
