{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
module Data.Aeson.Encoding.Internal
    (
    -- * Encoding
      Encoding' (..)
    , fromEncoding
    , Encoding
    , encodingToLazyByteString
    , unsafeToEncoding
    , retagEncoding
    , Series (..)
    , pairs
    , pair
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , wrapObject
    , wrapArray
    , null_
    , bool
    , text
    , lazyText
    , string
    , list
    , dict
    , tuple
    , (>*<)
    , InArray
    , empty
    , (><)
    , econcat
    -- ** Decimal numbers
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific
    -- ** Decimal numbers as Text
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText
    -- ** Time
    , day
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    -- ** value
    , value
    -- ** JSON tokens
    , comma, colon, openBracket, closeBracket, openCurly, closeCurly
    ) where

import Prelude        ()
import Prelude.Compat

import Data.ByteString.Builder      (Builder, char7, toLazyByteString)
import Data.ByteString.Builder.Prim (primBounded)
import Data.Int
import Data.Scientific              (Scientific)
import Data.Semigroup               (Semigroup ((<>)))
import Data.Text                    (Text)
import Data.Time                    (Day, LocalTime, TimeOfDay, UTCTime,
                                     ZonedTime)
import Data.Typeable                (Typeable)
import Data.Word

import Data.Aeson.Types.Internal (Value)

import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Text.Lazy              as LT

-- | An encoding of a JSON value.
--
-- @tag@ represents which kind of JSON the Encoding is encoding to,
-- we reuse 'Text' and 'Value' as tags here.
newtype Encoding' tag = Encoding { tokensEndo :: Tokens -> Tokens }
    deriving (Typeable)

-- | Acquire the underlying bytestring builder.
fromEncoding :: Encoding' a -> Builder
fromEncoding (Encoding e) = fromTokens (e TkEnd)

-- | Often used synonnym for 'Encoding''.
type Encoding = Encoding' Value

-- | Make Encoding from Builder.
--
-- Use with care! You have to make sure that the passed Builder
-- is a valid JSON Encoding!
unsafeToEncoding :: Builder -> Encoding' a
unsafeToEncoding = Encoding . TkUnsafe

encodingToLazyByteString :: Encoding' a -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding
{-# INLINE encodingToLazyByteString #-}

retagEncoding :: Encoding' a -> Encoding' b
retagEncoding = Encoding . tokensEndo

-------------------------------------------------------------------------------
-- Encoding instances
-------------------------------------------------------------------------------

{-
instance Show (Encoding' a) where
    show (Encoding e) = show (toLazyByteString e)

instance Eq (Encoding' a) where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord (Encoding' a) where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)
-}

-- | A series of values that, when encoded, should be separated by
-- commas. Since 0.11.0.0, the '.=' operator is overloaded to create
-- either @(Text, Value)@ or 'Series'. You can use Series when
-- encoding directly to a bytestring builder as in the following
-- example:
--
-- > toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
data Series = Empty
            | Value (Encoding' Series)
            deriving (Typeable)

pair :: Text -> Encoding -> Series
pair name val = Value $ retagEncoding $ text name >< colon >< val

instance Semigroup Series where
    Empty   <> a       = a
    a       <> Empty   = a
    Value a <> Value b = Value (a >< comma >< b)

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

nullEncoding :: Encoding' a -> Bool
nullEncoding (Encoding e) = case e TkEnd of
    TkEnd -> True
    _     -> False

emptyArray_ :: Encoding
emptyArray_ = Encoding TkEmptyArray

emptyObject_ :: Encoding
emptyObject_ = Encoding TkEmptyObject

wrapArray :: Encoding' a -> Encoding
wrapArray e = retagEncoding $ openBracket >< e >< closeBracket

wrapObject :: Encoding' a -> Encoding
wrapObject e = retagEncoding $ openCurly >< e >< closeCurly

null_ :: Encoding
null_ = Encoding TkNull

bool :: Bool -> Encoding
bool True = Encoding TkTrue
bool False = Encoding TkFalse

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs (Value v) = openCurly >< retagEncoding v >< closeCurly
pairs Empty     = emptyObject_
{-# INLINE pairs #-}

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
  where
    commas = foldr (\v vs -> comma >< to' v >< vs) empty
{-# INLINE list #-}

-- | Encode as JSON object
dict
    :: (k -> Encoding' Text)                     -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = pairs . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = retagEncoding (encodeKey k) >< colon >< retagEncoding (encodeVal v)
{-# INLINE dict #-}

-- | Type tag for tuples contents, see 'tuple'.
data InArray

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding' a -> Encoding' b -> Encoding' InArray
a >*< b = retagEncoding a >< comma >< retagEncoding b
{-# INLINE (>*<) #-}

empty :: Encoding' a
empty = Encoding id

econcat :: [Encoding' a] -> Encoding' a
econcat = foldr (><) empty

infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a . b)
{-# INLINE (><) #-}

-- | Encode as a tuple.
--
-- @
-- toEncoding (X a b c) = tuple $
--     toEncoding a >*<
--     toEncoding b >*<
--     toEncoding c
tuple :: Encoding' InArray -> Encoding
tuple b = retagEncoding $ openBracket >< b >< closeBracket
{-# INLINE tuple #-}

text :: Text -> Encoding' a
text = Encoding . TkText

lazyText :: LT.Text -> Encoding' a
lazyText = Encoding . TkLazyText

string :: String -> Encoding' a
string = Encoding . TkString

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding' a
comma        = Encoding TkComma
colon        = Encoding TkColon
openBracket  = Encoding TkOpenBracket
closeBracket = Encoding TkCloseBracket
openCurly    = Encoding TkOpenCurly
closeCurly   = Encoding TkCloseCurly

-------------------------------------------------------------------------------
-- Decimal numbers
-------------------------------------------------------------------------------

int8 :: Int8 -> Encoding
int8 = Encoding . TkInt8

int16 :: Int16 -> Encoding
int16 = Encoding . TkInt16

int32 :: Int32 -> Encoding
int32 = Encoding . TkInt32

int64 :: Int64 -> Encoding
int64 = Encoding . TkInt64

int :: Int -> Encoding
int = Encoding . TkInt

word8 :: Word8 -> Encoding
word8 = Encoding . TkWord8

word16 :: Word16 -> Encoding
word16 = Encoding . TkWord16

word32 :: Word32 -> Encoding
word32 = Encoding . TkWord32

word64 :: Word64 -> Encoding
word64 = Encoding . TkWord64

word :: Word -> Encoding
word = Encoding . TkWord

integer :: Integer -> Encoding
integer = Encoding . TkInteger

float :: Float -> Encoding
float = realFloatToEncoding (Encoding . TkFloat)

double :: Double -> Encoding
double = realFloatToEncoding (Encoding . TkDouble)

scientific :: Scientific -> Encoding
scientific = Encoding . TkScientific


realFloatToEncoding :: RealFloat a => (a -> Encoding) -> a -> Encoding
realFloatToEncoding e d
    | isNaN d || isInfinite d = null_
    | otherwise               = e d
{-# INLINE realFloatToEncoding #-}

-------------------------------------------------------------------------------
-- Decimal numbers as Text
-------------------------------------------------------------------------------

int8Text :: Int8 -> Encoding' a
int8Text = Encoding . TkInt8T

int16Text :: Int16 -> Encoding' a
int16Text = Encoding . TkInt16T

int32Text :: Int32 -> Encoding' a
int32Text = Encoding . TkInt32T

int64Text :: Int64 -> Encoding' a
int64Text = Encoding . TkInt64T

intText :: Int -> Encoding' a
intText = Encoding . TkIntT

word8Text :: Word8 -> Encoding' a
word8Text = Encoding . TkWord8T

word16Text :: Word16 -> Encoding' a
word16Text = Encoding . TkWord16T

word32Text :: Word32 -> Encoding' a
word32Text = Encoding . TkWord32T

word64Text :: Word64 -> Encoding' a
word64Text = Encoding . TkWord64T

wordText :: Word -> Encoding' a
wordText = Encoding . TkWordT

integerText :: Integer -> Encoding' a
integerText = Encoding . TkIntegerT

floatText :: Float -> Encoding' a
floatText = Encoding . TkFloatT

doubleText :: Double -> Encoding' a
doubleText = Encoding . TkDoubleT

scientificText :: Scientific -> Encoding' a
scientificText = Encoding . TkScientificT

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

day :: Day -> Encoding' a
day = Encoding . TkDay

localTime :: LocalTime -> Encoding' a
localTime = Encoding . TkLocalTime

utcTime :: UTCTime -> Encoding' a
utcTime = Encoding . TkUTCTime

timeOfDay :: TimeOfDay -> Encoding' a
timeOfDay = Encoding . TkTimeOfDay

zonedTime :: ZonedTime -> Encoding' a
zonedTime = Encoding . TkZonedTime

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

value :: Value -> Encoding
value = unsafeToEncoding . EB.encodeToBuilder

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

data Tokens
    = TkEnd
    -- empty objects
    | TkEmptyArray              Tokens
    | TkEmptyObject             Tokens
    | TkNull                    Tokens
    | TkTrue                    Tokens
    | TkFalse                   Tokens
    -- punctuation
    | TkComma                   Tokens
    | TkColon                   Tokens
    | TkOpenBracket             Tokens
    | TkCloseBracket            Tokens
    | TkOpenCurly               Tokens
    | TkCloseCurly              Tokens
    -- text
    | TkText        !Text       Tokens
    | TkLazyText    !LT.Text    Tokens
    | TkString      !String     Tokens
    -- unsafe
    | TkUnsafe      Builder     Tokens
    -- time
    | TkDay         !Day        Tokens
    | TkLocalTime   !LocalTime  Tokens
    | TkUTCTime     !UTCTime    Tokens
    | TkTimeOfDay   !TimeOfDay  Tokens
    | TkZonedTime   !ZonedTime  Tokens
    -- numbers
    | TkInt         !Int        Tokens
    | TkInt8        !Int8       Tokens
    | TkInt16       !Int16      Tokens
    | TkInt32       !Int32      Tokens
    | TkInt64       !Int64      Tokens
    | TkInteger     !Integer    Tokens
    | TkWord        !Word       Tokens
    | TkWord8       !Word8      Tokens
    | TkWord16      !Word16     Tokens
    | TkWord32      !Word32     Tokens
    | TkWord64      !Word64     Tokens
    | TkFloat       !Float      Tokens
    | TkDouble      !Double     Tokens
    | TkScientific  !Scientific Tokens
    -- numbers as text
    | TkIntT        !Int        Tokens
    | TkInt8T       !Int8       Tokens
    | TkInt16T      !Int16      Tokens
    | TkInt32T      !Int32      Tokens
    | TkInt64T      !Int64      Tokens
    | TkIntegerT    !Integer    Tokens
    | TkWordT       !Word       Tokens
    | TkWord8T      !Word8      Tokens
    | TkWord16T     !Word16     Tokens
    | TkWord32T     !Word32     Tokens
    | TkWord64T     !Word64     Tokens
    | TkFloatT      !Float      Tokens
    | TkDoubleT     !Double     Tokens
    | TkScientificT !Scientific Tokens

fromTokens :: Tokens -> Builder
fromTokens TkEnd = mempty
-- empty objects
fromTokens (TkEmptyArray  ts)  = EB.emptyArray_ <> fromTokens ts
fromTokens (TkEmptyObject ts)  = EB.emptyObject_ <> fromTokens ts
fromTokens (TkNull ts)         = EB.null_ <> fromTokens ts
fromTokens (TkTrue ts)         = "true" <> fromTokens ts
fromTokens (TkFalse ts)        = "false" <> fromTokens ts
-- punctuation
fromTokens (TkComma ts)        = char7 ',' <> fromTokens ts
fromTokens (TkColon ts)        = char7 ':' <> fromTokens ts
fromTokens (TkOpenBracket ts)  = char7 '[' <> fromTokens ts
fromTokens (TkCloseBracket ts) = char7 ']' <> fromTokens ts
fromTokens (TkOpenCurly ts)    = char7 '{' <> fromTokens ts
fromTokens (TkCloseCurly ts)   = char7 '}' <> fromTokens ts
-- text
fromTokens (TkText x ts)       = EB.text x <> fromTokens ts
fromTokens (TkLazyText lt ts)   =
    B.char7 '"' <>
    LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"' <> fromTokens ts) lt
fromTokens (TkString x ts)     = EB.string x <> fromTokens ts
-- unsafe
fromTokens (TkUnsafe x    ts)  = x <> fromTokens ts
-- time
fromTokens (TkDay x       ts)  = EB.quote (EB.day x) <> fromTokens ts
fromTokens (TkLocalTime x ts)  = EB.quote (EB.localTime x) <> fromTokens ts
fromTokens (TkUTCTime x   ts)  = EB.quote (EB.utcTime x) <> fromTokens ts
fromTokens (TkTimeOfDay x ts)  = EB.quote (EB.timeOfDay x) <> fromTokens ts
fromTokens (TkZonedTime x ts)  = EB.quote (EB.zonedTime x) <> fromTokens ts
-- numbers
fromTokens (TkInt x         ts) = B.intDec x <> fromTokens ts
fromTokens (TkInt8 x        ts) = B.int8Dec x <> fromTokens ts
fromTokens (TkInt16 x       ts) = B.int16Dec x <> fromTokens ts
fromTokens (TkInt32 x       ts) = B.int32Dec x <> fromTokens ts
fromTokens (TkInt64 x       ts) = B.int64Dec x <> fromTokens ts
fromTokens (TkInteger x     ts) = B.integerDec x <> fromTokens ts
fromTokens (TkWord x        ts) = B.wordDec x <> fromTokens ts
fromTokens (TkWord8 x       ts) = B.word8Dec x <> fromTokens ts
fromTokens (TkWord16 x      ts) = B.word16Dec x <> fromTokens ts
fromTokens (TkWord32 x      ts) = B.word32Dec x <> fromTokens ts
fromTokens (TkWord64 x      ts) = B.word64Dec x <> fromTokens ts
fromTokens (TkFloat x       ts) = B.floatDec x <> fromTokens ts
fromTokens (TkDouble x      ts) = B.doubleDec x <> fromTokens ts
fromTokens (TkScientific x  ts) = EB.scientific x <> fromTokens ts
-- numbers as text
fromTokens (TkIntT x         ts) = EB.quote (B.intDec x) <> fromTokens ts
fromTokens (TkInt8T x        ts) = EB.quote (B.int8Dec x) <> fromTokens ts
fromTokens (TkInt16T x       ts) = EB.quote (B.int16Dec x) <> fromTokens ts
fromTokens (TkInt32T x       ts) = EB.quote (B.int32Dec x) <> fromTokens ts
fromTokens (TkInt64T x       ts) = EB.quote (B.int64Dec x) <> fromTokens ts
fromTokens (TkIntegerT x     ts) = EB.quote (B.integerDec x) <> fromTokens ts
fromTokens (TkWordT x        ts) = EB.quote (B.wordDec x) <> fromTokens ts
fromTokens (TkWord8T x       ts) = EB.quote (B.word8Dec x) <> fromTokens ts
fromTokens (TkWord16T x      ts) = EB.quote (B.word16Dec x) <> fromTokens ts
fromTokens (TkWord32T x      ts) = EB.quote (B.word32Dec x) <> fromTokens ts
fromTokens (TkWord64T x      ts) = EB.quote (B.word64Dec x) <> fromTokens ts
fromTokens (TkFloatT x       ts) = EB.quote (B.floatDec x) <> fromTokens ts
fromTokens (TkDoubleT x      ts) = EB.quote (B.doubleDec x) <> fromTokens ts
fromTokens (TkScientificT x  ts) = EB.quote (EB.scientific x) <> fromTokens ts
