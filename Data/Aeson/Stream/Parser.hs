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
import qualified Data.HashMap.Strict as HM

-- import qualified Generics.SOP as SOP

-- Instances
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific

-- Unsafe interface
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

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

objectField :: FromStream a => Text -> OP a
objectField k = explicitObjectField k parseStream

objectFieldMaybe :: FromStream a => Text -> OP (Maybe a)
objectFieldMaybe k = explicitObjectFieldMaybe k parseStream

-------------------------------------------------------------------------------
-- Applicative Trie
-------------------------------------------------------------------------------

{--

data OP a where OP :: Maybe a -> Map.Map Text (OPF a) -> OP a
data OPF a where OPFAp :: P b -> OP (b -> a) -> OPF a

explicitObjectField :: Text -> P a -> OP a
explicitObjectField k p =
    OP Nothing (Map.singleton k (OPFAp p (pure id)))

explicitObjectFieldMaybe :: Text -> P a -> OP (Maybe a)
explicitObjectFieldMaybe k p =
    OP (Just Nothing) (Map.singleton k (OPFAp p (pure Just)))

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

            -- should never happen
            t -> fail $ "Expected key in " ++ name ++ ", got " ++ show t
--}--

-------------------------------------------------------------------------------
-- "Unsafe" Object
-------------------------------------------------------------------------------

-- | Unsafe object parser.
--
-- First step: we can differentiate singleton cases
-- This done because in cases f <$> x and f <*> x, we won't need to modify
-- pure function.
data UOP a where
    UOP             :: U a -> UOP a
    USingleton      :: Text -> P a -> UOP a
    USingletonMaybe :: Text -> P a -> UOP (Maybe a)

-- | Unsafe object parser. The thing
data U a = U
    { uF :: !Any                  -- ^ function
    , uK :: V.Vector Text         -- ^ keys, in order function takes arguments.
    , uP :: Map.Map Text (P Any)  -- ^ parsers
    , uD :: Map.Map Text Any      -- ^ default values
    }

explicitObjectFieldU :: Text -> P a -> UOP a
explicitObjectFieldU = USingleton

explicitObjectFieldMaybeU :: Text -> P a -> UOP (Maybe a)
explicitObjectFieldMaybeU = USingletonMaybe

instance Functor UOP where
    fmap f (UOP u) = UOP (fmap f u)
    fmap f (USingleton k p) = UOP U
        { uF = unsafeCoerce f
        , uK = V.singleton k
        , uP = Map.singleton k (unsafeCoerce p)
        , uD = Map.empty
        }
    fmap f (USingletonMaybe k p) = UOP U
        { uF = unsafeCoerce f
        , uK = V.singleton k
        , uP = Map.singleton k (unsafeCoerce (fmap Just p))
        , uD = Map.singleton k (unsafeCoerce (Nothing :: Maybe ()))
        }

toU :: forall a. UOP a -> U a
toU (UOP u) = u
toU (USingleton k p) = U
    { uF = unsafeCoerce (id :: a -> a)
    , uK = V.singleton k
    , uP = Map.singleton k (unsafeCoerce p)
    , uD = Map.empty
    }
toU (USingletonMaybe k p) = U
    { uF = unsafeCoerce (id :: a -> a)
    , uK = V.singleton k
    , uP = Map.singleton k (unsafeCoerce (fmap Just p))
    , uD = Map.singleton k (unsafeCoerce (Nothing :: Maybe ()))
    }

instance Applicative UOP where
    pure = UOP . pure
    UOP (U f k p d) <*> USingleton kx kp = UOP U
        { uF = f
        , uK = V.snoc k kx -- todo check, kx not in k
        , uP = Map.insert kx (unsafeCoerce kp) p
        , uD = d
        }
    UOP (U f k p d) <*> USingletonMaybe kx kp = UOP U
        { uF = f
        , uK = V.snoc k kx -- todo check, kx not in k
        , uP = Map.insert kx (unsafeCoerce (fmap Just kp)) p
        , uD = Map.insert kx (unsafeCoerce (Nothing :: Maybe ())) d
        }
    u <*> v = UOP (toU u <*> toU v)

instance Functor U where
    fmap f _ = error "fmap @U: not implemented"

instance Applicative U where
    pure x = U
        { uF = unsafeCoerce x
        , uK = V.empty
        , uP = Map.empty
        , uD = Map.empty
        }
    _ <*> _ = error "(<*>) @U: not implemented"

withObjectPU :: forall a. String -> UOP a -> P a
withObjectPU name op = open >> pre op
  where
    open :: P ()
    open = P $ \ts e k -> case ts of
        (TkObjectOpen : ts') -> k () ts'
        (t : _) -> e $ "Expected object " ++ name ++ ", got " ++ show t
        []      -> e $ "Expected object " ++ name ++ ", got end-of-file"

    {- close not needed
    close :: forall b. b -> P b
    close x = P $ \ts e k -> case ts of
        (TkObjectClose : ts') -> k x ts'
        -- extra keys!
        (TkKey _ : ts') ->
            runP skipValue ts'  e $ \() ts'' ->
            runP (close x) ts'' e k
        (t : _) -> e $ "Expected '}' " ++ name ++ ", got " ++ show t
        []      -> e $ "Expected '}' " ++ name ++ ", got end-of-file"
    -}

    pre :: forall b. UOP b -> P b
    pre (UOP (U f k p d)) = go f k p d
    pre _ = error "USIngleton/Maybe not handled"

    go :: forall b. Any -> V.Vector Text -> Map.Map Text (P Any) -> Map.Map Text Any -> P b
    go f ks ps acc = do
        t <- token $ "key in " ++ name
        case t of
            TkKey k -> case Map.lookup k ps of
                Nothing -> skipValue >> go f ks ps acc
                Just p  -> do
                    x <- unsafeCoerce p :: P ()
                    go f ks ps (Map.insert k (unsafeCoerce x) acc)

            TkObjectClose
                | vlen /= Map.size acc ->
                    fail $ "Unexpected '}', while parsing incomplete " ++ name
                | otherwise -> case vlen of
                    0 -> pure (unsafeCoerce acc)
                    1 -> postAcc1 f (V.unsafeIndex ks 0) acc
                    2 -> postAcc2 f (V.unsafeIndex ks 1) (V.unsafeIndex ks 2) acc
                    _ -> foldAcc f (V.toList ks) acc
              where
                vlen = V.length ks

            -- should never happen
            t -> fail $ "Expected key in " ++ name ++ ", got " ++ show t

    foldAcc :: forall b. Any -> [Text] -> Map.Map Text Any -> P b
    foldAcc x [] _ = pure (unsafeCoerce x)
    foldAcc f (k : ks) acc
        | Just v <- Map.lookup k acc = foldAcc (unsafeCoerce f v) ks acc
        | otherwise = fail $ "U inconsistency: doesn't exist " ++ show k

    postAcc1 :: forall b. Any -> Text -> Map.Map Text Any -> P b
    postAcc1 f k1 acc
        | Just v1 <- Map.lookup k1 acc = pure (unsafeCoerce f v1)
        | otherwise = fail $ "U inconsistency: doesn't exist " ++ show k1

    postAcc2 :: forall b. Any -> Text -> Text -> Map.Map Text Any -> P b
    postAcc2 f k1 k2 acc
        | Just v1 <- Map.lookup k1 acc
        , Just v2 <- Map.lookup k2 acc = pure (unsafeCoerce f v1 v2)
        | otherwise = fail $ "U inconsistency: doesn't exist " ++ show k1 ++ " or " ++ show k2

--{--

type OP = UOP

explicitObjectField :: Text -> P a -> UOP a
explicitObjectField = explicitObjectFieldU

explicitObjectFieldMaybe :: Text -> P a -> UOP (Maybe a)
explicitObjectFieldMaybe = explicitObjectFieldMaybeU

withObjectP :: forall a. String -> OP a -> P a
withObjectP = withObjectPU

--}--

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
