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
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

-- import qualified Generics.SOP as SOP

-- Instances
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific

--
import Control.Monad.ST
import qualified Data.Vector.Mutable as MV

-- Unsafe interface
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- import Debug.Trace

newtype P s a = P
    { runP :: forall b. TokenStream
    -> (String -> ST s (Either String b))           -- type Failure b =
    -> (a -> TokenStream -> ST s (Either String b)) -- type Success a b =
    -> ST s (Either String b)
    }

instance Functor (P s) where
    fmap f (P p) = P $ \ts e s -> p ts e (s . f)

instance Applicative (P s) where
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

instance Monad (P s) where
    return = pure
    P p >>= f = P $ \ts e k -> p ts e $ \a ts' -> runP (f a) ts' e k
    P p >> P q = P $ \ts e k -> p ts e $ \_ ts' -> q ts' e k

    fail err = P $ \_ e _ -> e err

token :: String -> P s Token
token name = P $ \ts e k -> case ts of
    []        -> e $ "Unexpected end-of-file, expecting: " ++ name
    (t : ts') -> k t ts'

peekToken :: String -> P s Token
peekToken name = P $ \ts e k -> case ts of
    []      -> e $ "Unexpected end-of-file, expecting: " ++ name
    (t : _) -> k t ts

expectedToken :: Token -> P s ()
expectedToken t = P $ \ts e k -> case ts of
    (t' : ts')
        | t == t'   -> k () ts'
        | otherwise -> e $ "Unexpected " ++ show t' ++ ", expecting" ++ show t
    []              -> e $ "Unexpected end-of-file, expecting: " ++ show t

liftST :: ST s a -> P s a
liftST st = P $ \ts _e k -> st >>= \x -> k x ts

-------------------------------------------------------------------------------
-- Object
-------------------------------------------------------------------------------

objectField :: FromStream a => Text -> OP s a
objectField k = explicitObjectField k parseStream

objectFieldMaybe :: FromStream a => Text -> OP s (Maybe a)
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

{--

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
--}--

-------------------------------------------------------------------------------
-- Unsafe Object parsing in ST monad
-------------------------------------------------------------------------------

{-
data OP s a where
    UOP             :: U s a -> OP s a
    USingleton      :: Text -> P s a -> OP s a
    USingletonMaybe :: Text -> P s a -> OP s (Maybe a)

-- | Unsafe object parser. The thing
data U s a = U
    { uF :: !Any                         -- ^ function
    , uP :: Map.Map Text (Int, P s Any)  -- ^ parsers, their index.
    , uM :: V.Vector Bool                -- ^ mask, which arguments we have
    , uD :: V.Vector Any                 -- ^ default arguments, "init" of the loop.
    }

explicitObjectField :: Text -> P s a -> OP s a
explicitObjectField = USingleton

explicitObjectFieldMaybe :: Text -> P s a -> OP s (Maybe a)
explicitObjectFieldMaybe = USingletonMaybe

instance Functor (OP s) where
    fmap f (UOP u) = UOP (fmap f u)
    fmap f (USingleton k p) = UOP U
        { uF = unsafeCoerce f
        , uP = Map.singleton k (0, unsafeCoerce p)
        , uM = V.singleton False
        , uD = V.singleton anyUnit
        }
    fmap f (USingletonMaybe k p) = UOP U
        { uF = unsafeCoerce f
        , uP = Map.singleton k (0, unsafeCoerce (fmap Just p))
        , uM = V.singleton True
        , uD = V.singleton anyNothing
        }

toU :: forall a s. OP s a -> U s a
toU (UOP u) = u
toU (USingleton k p) = U
    { uF = unsafeCoerce (id :: a -> a)
    , uP = Map.singleton k (0, unsafeCoerce p)
    , uM = V.singleton False
    , uD = V.singleton anyUnit
    }
toU (USingletonMaybe k p) = U
    { uF = unsafeCoerce (id :: a -> a)
    , uP = Map.singleton k (0, unsafeCoerce (fmap Just p))
    , uM = V.singleton True
    , uD = V.singleton anyNothing
    }

instance Applicative (OP s) where
    pure = UOP . pure
    UOP (U f p m d) <*> USingleton kx kp = UOP U
        { uF = f
        , uP = Map.insert kx (V.length m, unsafeCoerce kp) p
        , uM = V.snoc m False
        , uD = V.snoc d anyUnit
        }
    UOP (U f p m d) <*> USingletonMaybe kx kp = UOP U
        { uF = f
        , uP = Map.insert kx (V.length m, unsafeCoerce (fmap Just kp)) p
        , uM = V.snoc m True
        , uD = V.snoc d anyNothing
        }

    u <*> v = UOP (toU u <*> toU v)

instance Functor (U s) where
    fmap f _ = error "fmap @U: not implemented"

instance Applicative (U s) where
    pure x = U
        { uF = unsafeCoerce x
        , uP = Map.empty
        , uM = V.empty
        , uD = V.empty
        }
    _ <*> _ = error "(<*>) @U: not implemented"


withObjectP :: forall a s. String -> OP s a -> P s a
withObjectP name op = open >> pre op
  where
    open :: P s ()
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

    pre :: forall b. OP s b -> P s b
    pre (UOP (U f p m d)) = do
        mask <- liftST (V.thaw m)
        acc  <- liftST (V.thaw d)
        go f p mask acc
    pre _ = error "USIngleton/Maybe not handled"

    go :: forall b. Any -> Map.Map Text (Int, P s Any) -> V.MVector s Bool -> V.MVector s Any -> P s b
    go f ps mask acc = do
        t <- token $ "key in " ++ name
        case t of
            TkKey k -> case Map.lookup k ps of
                Nothing -> skipValue >> go f ps mask acc
                Just (idx, p)  -> do
                    x <- unsafeCoerce p :: P s ()
                    liftST $ do
                        MV.unsafeWrite mask idx True
                        MV.unsafeWrite acc idx (unsafeCoerce x)
                    go f ps mask acc

            TkObjectClose -> do
                mask' <- liftST $ V.freeze mask
                case and mask' of
                    False -> fail $ "Unexpected '}', while parsing incomplete " ++ name
                    True -> do
                        acc' <- liftST $ V.freeze acc
                        foldAcc f (V.toList acc')

            -- should never happen
            t -> fail $ "Expected key in " ++ name ++ ", got " ++ show t

    foldAcc :: forall b. Any -> [Any] -> P s b
    foldAcc x []       = pure (unsafeCoerce x)
    foldAcc f (v : vs) = foldAcc (unsafeCoerce f v) vs
--}--

-------------------------------------------------------------------------------
-- Simpler Unsafe ST variant
-------------------------------------------------------------------------------

data OP s a = OP
    { uF :: Int -> V.MVector s Any -> ST s a   -- ^ function
    , uP :: !(Map.Map Text (SP s))             -- ^ parsers, their index.
    , uM :: !(V.Vector Bool)                   -- ^ mask, which arguments we have
    , uD :: !(V.Vector Any)                    -- ^ default arguments, "init" of the loop.
    }

data SP s = SP !Int !(P s Any)

explicitObjectField :: Text -> P s a -> OP s a
explicitObjectField k p = OP
    { uF = opF
    , uP = Map.singleton k (SP 0 (unsafeCoerce p))
    , uM = V.singleton False
    , uD = V.singleton anyUnit
    }

opF :: Int -> V.MVector s Any -> ST s a
opF i mv = fmap unsafeCoerce (MV.unsafeRead mv i)

explicitObjectFieldMaybe :: Text -> P s a -> OP s (Maybe a)
explicitObjectFieldMaybe k p = OP
    { uF = opF
    , uP = Map.singleton k (SP 0 (unsafeCoerce (fmap Just p)))
    , uM = V.singleton True
    , uD = V.singleton anyNothing
    }

instance Functor (OP s) where
    fmap g (OP f p m d) = OP f' p m d
      where
        f' i mv = fmap g (f i mv)

instance Applicative (OP f) where
    pure x = OP
        { uF = f
        , uP = mempty
        , uM = mempty
        , uD = mempty
        }
      where
        f _ _ = pure x

    OP ff fp fm fd <*> OP xf xp xm xd = OP
        { uF = f
        , uP = Map.union fp xp'
        , uM = (V.++) fm xm
        , uD = (V.++) fd xd
        }
      where
        n = V.length fm
        xp' = fmap (first (+ n)) xp

        f i mv = ff i mv <*> xf (i + n) mv

first :: (Int -> Int) -> SP s -> SP s
first f (SP i x) = SP (f i) x

withObjectP :: forall a s. String -> OP s a -> P s a
withObjectP name op = open >> pre op
  where
    open :: P s ()
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

    pre :: forall b. OP s b -> P s b
    pre (OP f p m d) = do
        mask <- liftST (V.thaw m)
        acc  <- liftST (V.thaw d)
        go f p mask acc

    go :: forall b. (Int -> V.MVector s Any -> ST s b)
       -> Map.Map Text (SP s)
       -> V.MVector s Bool
       -> V.MVector s Any
       -> P s b
    go f ps mask acc = do
        t <- token $ "key in " ++ name
        case t of
            TkKey k -> case Map.lookup k ps of
                Nothing -> skipValue >> go f ps mask acc
                Just (SP idx p) -> do
                    x <- unsafeCoerce p :: P s ()
                    liftST $ do
                        MV.unsafeWrite mask idx True
                        MV.unsafeWrite acc idx (unsafeCoerce x)
                    go f ps mask acc

            TkObjectClose -> do
                mask' <- liftST $ V.freeze mask
                case and mask' of
                    False -> fail $ "Unexpected '}', while parsing incomplete " ++ name
                    True -> liftST (f 0 acc)

            -- should never happen
            t -> fail $ "Expected key in " ++ name ++ ", got " ++ show t

-------------------------------------------------------------------------------
-- Any's
-------------------------------------------------------------------------------

anyUnit :: Any
anyUnit = unsafeCoerce ()

anyNothing :: Any
anyNothing = unsafeCoerce (Nothing :: Maybe ())

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

foldArray' :: forall a b s. b -> (b -> a -> b) -> P s a -> P s b
foldArray' z f p = open >>= go
  where
    open :: P s b
    open = P $ \ts e k -> case ts of
        (TkArrayOpen : ts') -> k z ts'
        (t : _) -> e $ "Expected array, got " ++ show t
        []      -> e "Expected array, got end-of-file"

    go :: b -> P s b
    go b = P $ \ts e k -> case ts of
        (TkArrayClose : ts') -> k b ts'
        ts'@(_ : _) ->
            runP p ts' e $ \a ts'' ->
            let b' = f b a in b' `seq` runP (go b') ts'' e k
        [] -> e "Expected array item or ']', got end-of-file"

skipValue :: P s ()
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

valueP :: P s Value
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
    record :: P s [Pair]
    record = do
        t <- token "record key"
        case t of
            TkKey k -> do
                v <- valueP
                fmap (k .= v :) record
            TkObjectClose -> pure []
            _ -> fail $ "Expecting record key or '}', got token " ++ show t

    -- use foldArray'?
    array :: P s [Value]
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
    parseStream :: P s a

-- | rename to parseStreamViaValue?
fromParseJSON :: FromJSON a => P s a
fromParseJSON = withValueE f
  where
    f v = case fromJSON v of
        Success x -> Right x
        Error err -> Left err

decodeStream :: FromStream a => TokenStream -> Either String a
decodeStream stream = runST (runP parseStream stream (pure . Left) success)
  where
    success x [] = pure (Right x)
    success _ ts = pure (Left $ "Unconsumed input: " ++ show (take 10 ts))

-------------------------------------------------------------------------------
-- with-
-------------------------------------------------------------------------------

withValueE :: (Value -> Either String a) -> P s a
withValueE f = do
    v <- valueP
    case f v of
        Right x  -> pure x
        Left err -> fail err

withScientificE :: (Scientific -> Either String a) -> P s a
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

parseBoundedIntegral:: (Bounded a, Integral a) => P s a
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
