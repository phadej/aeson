{-# LANGUAGE RankNTypes, GADTs, ScopedTypeVariables, BangPatterns #-}
module Data.Aeson.Stream.Parser where

import Control.Monad (ap)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Function ((&))

import Data.Aeson.Types
import Data.Aeson.Stream

import Data.Int (Int64)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Vector as V

-- import qualified Generics.SOP as SOP

-- Instances
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific

-- import Debug.Trace

newtype P a = P
    { runP :: forall b. TokenStream
    -> (String -> Either String b)           -- type Failure b =
    -> (a -> TokenStream -> Either String b) -- type Success a b =
    -> Either String b
    }

instance Functor P where
    fmap f (P p) = P $ \ts e s -> p ts e (s . f)

instance Applicative P where
    pure x = P $ \ts _ s -> s x ts
    P pf <*> P px = P $ \ts e k ->
        pf ts  e $ \f ts'  ->
        px ts' e $ \x ts'' ->
        k (f x) ts''
    P p *> P q = P $ \ts e k -> p ts e $ \_ ts' -> q ts' e k
    P p <* P q = P $ \ts e k ->
        p ts  e $ \x ts'  ->
        q ts' e $ \_ ts'' ->
        k x ts''

instance Monad P where
    return = pure
    P p >>= f = P $ \ts e k -> p ts e $ \a ts' -> runP (f a) ts' e k
    P p >> P q = P $ \ts e k -> p ts e $ \_ ts' -> q ts' e k

    fail err = P $ \_ e _ -> e err

token :: String -> P Token
token name = P $ \ts e k -> case ts of
    []        -> e $ "Unexpected end-of-file, expecting: " ++ name
    (t : ts') -> k t ts'

peekToken :: String -> P Token
peekToken name = P $ \ts e k -> case ts of
    []      -> e $ "Unexpected end-of-file, expecting: " ++ name
    (t : _) -> k t ts

expectedToken :: Token -> P ()
expectedToken t = P $ \ts e k -> case ts of
    (t' : ts')
        | t == t'   -> k () ts'
        | otherwise -> e $ "Unexpected " ++ show t' ++ ", expecting" ++ show t
    []              -> e $ "Unexpected end-of-file, expecting: " ++ show t

-------------------------------------------------------------------------------
-- Object
-------------------------------------------------------------------------------

-- | Applicative Trie!
data OP a where OP :: Maybe a -> Map.Map Text (OPF a) -> OP a
data OPF a where OPFAp :: P b -> OP (b -> a) -> OPF a

instance Functor OPF where
    fmap f (OPFAp p op) = OPFAp p (fmap (f .) op)

instance Functor OP where
    fmap f (OP m opf) = OP (fmap f m) (fmap (fmap f) opf)

instance Applicative OP where
    pure x = OP (Just x) Map.empty
    f@(OP fo fm) <*> x@(OP xo xm) = OP (fo <*> xo) $ Map.union
        (combine ($) fm x)
        (combine (&) xm f)
      where
        combine :: (a -> b -> c) -> Map.Map Text (OPF a) -> OP b -> Map.Map Text (OPF c)
        combine g m op = fmap (flip (combine' g) op) m
        {-# INLINE combine #-}

        combine' :: (a -> b -> c) -> OPF a -> OP b -> OPF c
        combine' g (OPFAp p op) op' = OPFAp p (h <$> op <*> op')
          where
            h da b d = g (da d) b
        {-# INLINE combine' #-}

explicitObjectField :: Text -> P a -> OP a
explicitObjectField k p =
    OP Nothing (Map.singleton k (OPFAp p (pure id)))

explicitObjectFieldMaybe :: Text -> P a -> OP (Maybe a)
explicitObjectFieldMaybe k p =
    OP (Just Nothing) (Map.singleton k (OPFAp p (pure Just)))

objectField :: FromStream a => Text -> OP a
objectField k = explicitObjectField k parseStream

objectFieldMaybe :: FromStream a => Text -> OP (Maybe a)
objectFieldMaybe k = explicitObjectFieldMaybe k parseStream

withObjectP :: forall a. String -> OP a -> P a
withObjectP name op = open >> go op
  where
    open :: P ()
    open = P $ \ts e k -> case ts of
        (TkObjectOpen : ts') -> k () ts'
        (t : _) -> e $ "Expected object " ++ name ++ ", got " ++ show t
        []      -> e $ "Expected object " ++ name ++ ", got end-of-file"

    close :: forall b. b -> P b
    close x = P $ \ts e k -> case ts of
        (TkObjectClose : ts') -> k x ts'
        -- extra keys!
        (TkKey _ : ts') ->
            runP skipValue ts'  e $ \() ts'' ->
            runP (close x) ts'' e k
        (t : _) -> e $ "Expected '}' " ++ name ++ ", got " ++ show t
        []      -> e $ "Expected '}' " ++ name ++ ", got end-of-file"

    go :: forall b. OP b -> P b
    go op@(OP o m) = do
        t <- token $ "key in " ++ name
        case t of
            TkKey k -> case Map.lookup k m of
                -- unknown key
                Nothing            -> skipValue >> go op
                Just (OPFAp p op') -> (&) <$> p <*> go op'
            TkObjectClose -> case o of
                Just x   -> pure x
                Nothing  -> fail $ "Unexpected '}', while parsing incomplete " ++ name ++ "; missing keys " ++ show (Map.keys m)
            t -> fail $ "Expected key in " ++ name ++ ", got " ++ show t

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

foldArray' :: forall a b. b -> (b -> a -> b) -> P a -> P b
foldArray' z f p = open >>= go
  where
    open :: P b
    open = P $ \ts e k -> case ts of
        (TkArrayOpen : ts') -> k z ts'
        (t : _) -> e $ "Expected array, got " ++ show t
        []      -> e "Expected array, got end-of-file"

    go :: b -> P b
    go b = P $ \ts e k -> case ts of
        (TkArrayClose : ts') -> k b ts'
        ts'@(_ : _) ->
            runP p ts' e $ \a ts'' ->
            let b' = f b a in b' `seq` runP (go b') ts'' e k
        [] -> e "Expected array item or ']', got end-of-file"

skipValue :: P ()
skipValue = P impl
  where
    impl ts e k = go (0 :: Int) ts
      where
        go _ [] = e "Unexpected end-of-input, while skipping JSON value"
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
            TkError err   -> e err

        done !n ts | n <= 0    = k () ts
                   | otherwise = go n ts

valueP :: P Value
valueP = do
    t <- token "Start of JSON value"
    case t of
        TkObjectOpen -> fmap object record
        TkArrayOpen  -> fmap mkArray array
        TkText x     -> pure (String x)
        TkNumber x   -> pure (Number x)
        TkNull       -> pure Null
        TkTrue       -> pure (Bool True)
        TkFalse      -> pure (Bool False)
        _ -> fail $ "Expecting JSON value, got token " ++ show t
  where
    record :: P [Pair]
    record = do
        t <- token "record key"
        case t of
            TkKey k -> do
                v <- valueP
                fmap (k .= v :) record
            TkObjectClose -> pure []
            _ -> fail $ "Expecting record key or '}', got token " ++ show t

    -- use foldArray'?
    array :: P [Value]
    array = do
        t <- peekToken "JSON value or ']'"
        case t of
            TkArrayClose -> token "']'" >> pure []
            _ -> (:) <$> valueP <*> array

    mkArray :: [Value] -> Value
    mkArray = Array . V.fromList

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class FromStream a where
    parseStream :: P a

-- | rename to parseStreamViaValue?
fromParseJSON :: FromJSON a => P a
fromParseJSON = withValueE f
  where
    f v = case fromJSON v of
        Success x -> Right x
        Error err -> Left err

decodeStream :: FromStream a => TokenStream -> Either String a
decodeStream stream = runP parseStream stream Left success
  where
    success x [] = Right x
    success _ ts = Left $ "Unconsumed input: " ++ show (take 10 ts)

-------------------------------------------------------------------------------
-- with-
-------------------------------------------------------------------------------

withValueE :: (Value -> Either String a) -> P a
withValueE f = do
    v <- valueP
    case f v of
        Right x  -> pure x
        Left err -> fail err

withScientificE :: (Scientific -> Either String a) -> P a
withScientificE f = P $ \ts e k -> case ts of
    (TkNumber x : ts') -> case f x of
        Right y  -> k y ts'
        Left err -> e err
    (t : _) -> e $ "Expected text-value, got " ++ show t
    []      -> e "Expected text-value, got end-of-file"

-------------------------------------------------------------------------------
-- LL(1)
-------------------------------------------------------------------------------

-- | LL(1) Parser to make TokenStream -> Value transform.
newtype P1 a = P1
    { runP1 :: forall b. TokenStream
    -> (a -> TokenStream -> Either String b)
    -> Either String b
    }

instance Functor P1 where
    fmap f (P1 p) = P1 $ \ts k -> p ts (k . f)

instance Applicative P1 where
    pure x = P1 $ \ts k -> k x ts
    (<*>) = ap

instance Monad P1 where
    return = pure
    P1 p >>= f = P1 $ \ts k -> p ts $ \a ts' -> runP1 (f a) ts' k

    fail err = P1 $ \_ _ -> Left err

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- TODO: parseRealFloat, with Null -> (0/0)
instance FromStream Double where
    parseStream = withScientificE $
        Right . Scientific.toRealFloat

parseBoundedIntegral:: (Bounded a, Integral a) => P a
parseBoundedIntegral = withScientificE $ \s -> maybe
    (Left $ "??? is either floating or will cause over or underflow: " ++ show s)
    Right
    (Scientific.toBoundedInteger s)

instance FromStream Int where
    parseStream = parseBoundedIntegral

instance FromStream Int64 where
    parseStream = parseBoundedIntegral

instance FromStream Text where
    parseStream = P $ \ts e k -> case ts of
        (TkText x : ts') -> k x ts'
        (t : _) -> e $ "Expected text-value, got " ++ show t
        []      -> e "Expected text-value, got end-of-file"

instance (FromStream a, Ord a) => FromStream (Set.Set a) where
    parseStream = foldArray' Set.empty (\s i -> Set.insert i s) parseStream

instance FromStream a => FromStream [a] where
    parseStream = ($ []) <$>
        foldArray' id (\xs x -> xs . (x :)) parseStream

instance FromStream a => FromStream (Maybe a) where
    parseStream = P $ \ts e k -> case ts of
        (TkNull : ts') -> k Nothing ts'
        _              -> runP parseStream ts e $ \x ts' -> k (Just x) ts'

instance (FromStream a, FromStream b) => FromStream (a, b) where
    parseStream = do
        expectedToken TkArrayOpen
        a <- parseStream
        b <- parseStream
        (a, b) <$ expectedToken TkArrayClose
