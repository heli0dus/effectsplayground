
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module Effect.Scoped where
import Control.Monad (ap)
import GHC.RTS.Flags (RTSFlags(profilingFlags))

type f ~> g = forall a. f a -> g a

data HExc e m a =
    Throw' e
    | forall x. Catch' (m x) (e -> m x) (x -> m a)

class HFunctor h where
    hmap:: (Functor f, Functor g) => (f ~> g) -> (h f ~> h g)

instance HFunctor (HExc e) where
    hmap _ (Throw' e) = Throw' e
    hmap f (Catch' error handler cont) = Catch' (f error) (f . handler) (f . cont)

data Prog sig a =
    Return a
    | Op (sig (Prog sig) a)

instance Functor (sig (Prog sig)) => Functor (Prog sig) where
    fmap f (Return a) = Return (f a)
    fmap f (Op op) = Op $ f <$> op

type Handler s m n = forall x. s (m x) -> n (s x)

class (HFunctor sig) => Syntax sig where
    emap::(m a -> m b) -> (sig m a -> sig m b)
    weave::(Monad m, Monad s, Functor s) => s () -> Handler s m n -> (sig m a -> sig n (s a))

instance (Syntax sig, Functor (sig (Prog sig))) => Applicative (Prog sig) where
    pure = Return
    (<*>) = ap


instance (Syntax sig, Functor (sig (Prog sig))) => Monad (Prog sig) where
    Return v >>= prog = prog v
    Op op >>= prog = Op $ emap (>>= prog) op

instance Syntax (HExc e) where
    emap f (Throw' e) = Throw' e
    emap f (Catch' prog hdl cont) = Catch' prog hdl (f . cont)
    weave f hdl (Throw' e) = Throw' e
    weave f hdl (Catch' prog hdl' cont) = Catch'
        (hdl (prog <$ f))
        (\e -> hdl (hdl' e <$ f))
        (hdl . fmap cont)


----- data types -------------

data (sig :+: sig') m a = L (sig m a) | R (sig' m a)

instance (Functor (sig m), Functor (sig' m)) => Functor ((sig :+: sig') m) where
    fmap f (L op) = L $ fmap f op
    fmap f (R op) = R $ fmap f op

instance (HFunctor sig1, HFunctor sig2) => HFunctor (sig1 :+: sig2) where
    hmap f = \case
        (L prog) -> L (hmap f prog)
        (R prog) -> R (hmap f prog)

instance (Syntax sig1, Syntax sig2) => Syntax (sig1 :+: sig2) where
    emap hf = \case
        (L op) -> L $ emap hf op
        (R op) -> R $ emap hf op
    weave s handler = \case
        (L op) -> L $ weave s handler op
        (R op) -> R $ weave s handler op

newtype (Lift sig) m a = Lift (sig (m a))
    deriving Functor

instance (Functor sig) => HFunctor (Lift sig) where
    hmap f (Lift op) = Lift $ f <$> op

instance (Functor sig) => Syntax (Lift sig) where
    emap f (Lift op) = Lift $ f <$> op
    weave s hdl (Lift op) = Lift $ fmap (\p -> hdl (p <$ s)) op

class (Syntax sub, Syntax sup) => sub :>> sup where
    inj :: sub m a -> sup m a
    prj :: sup m a -> Maybe (sub m a)

instance (Syntax sig) => sig :>> sig where
    inj = id
    prj = Just

instance {-# OVERLAPPING #-} (Syntax sig1, Syntax sig2) => sig1 :>> (sig1 :+: sig2) where
    inj = L
    prj (L prog) = Just prog
    prj _ = Nothing

instance {-# OVERLAPPING #-} (Syntax sig1, Syntax sig2, sig :>> sig2) => sig :>> (sig1 :+: sig2) where
    inj = R . inj
    prj = \case
        R prog -> prj prog
        L _ -> Nothing

inject:: (sub :>> sup) => sub (Prog sup) a -> Prog sup a
inject = Op . inj

project:: (sub :>> sup) => Prog sup a -> Maybe (sub (Prog sup) a)
project = \case
    (Return x) -> Nothing
    (Op s) -> prj s

----------- Exceptions

pattern Throw :: (HExc e :>> sup) => e -> Prog sup a
pattern Throw e <- (project -> Just (Throw' e))
throw :: (HExc e :>> sig) => e -> Prog sig a
throw e = inject (Throw' e)

pattern Catch :: (HExc e :>> sup) => Prog sup x -> (e -> Prog sup x) -> (x -> Prog sup a) -> Prog sup a
pattern Catch p h k <- (project -> Just (Catch' p h k))
catch::(HExc e :>> sig, Functor (sig (Prog sig))) => Prog sig a -> (e -> Prog sig a) -> Prog sig a
catch prog handler = inject (Catch' prog handler pure)

------------- State -------------

pattern Other s = Op (R s)

data State s cnt =
    Get' (s -> cnt)
    | Put' s cnt
    deriving Functor

type HState s = Lift (State s)

pattern Get :: (Lift (State s) :>> sup) => (s -> Prog sup a) -> Prog sup a
pattern Get k <- (project -> Just ( Lift (Get' k)) )

get::(HState s :>> sig, Functor (sig (Prog sig))) => Prog sig s
get = inject (Lift (Get' pure))

pattern Put :: (Lift (State s) :>> sup) => s -> Prog sup a -> Prog sup a
pattern Put s cnt <- (project -> Just (Lift (Put' s cnt)))
put:: (HState s :>> sig, Functor (sig (Prog sig))) => s -> Prog sig ()
put s = inject (Lift (Put' s (pure ())))

runState:: (Syntax sig) => s -> Prog (HState s :+: sig) a -> Prog sig (s, a)
runState s = \case
    (Return a) -> Return (s, a)
    (Get k) -> runState s (k s)
    (Put s' k) -> runState s' k
    (Other op) -> Op $ weave (s, ()) (uncurry runState) op
    _ -> undefined