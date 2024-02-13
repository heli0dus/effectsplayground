{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}

module Threading where
import Scoped
import Control.Monad (liftM, join)

data Thread m a =
    Yield' (m a)
    | forall x. Fork' (m x) (m a)

instance HFunctor Thread where
    hmap f (Yield' p) = Yield' (f p)
    hmap f (Fork' ch par) = Fork' (f ch) (f par)

instance Syntax Thread where
    emap f = \case
        (Yield' p) -> Yield' (f p)
        (Fork' ch par) -> Fork' ch (f par)

    weave s hdl = \case
        (Yield' p) -> Yield' $ hdl (p <$ s)
        (Fork' ch par) -> Fork' (hdl (ch <$ s))
                                (hdl (par <$ s))

data Daemon sig = forall x. Daemon (Prog (Thread :+: sig) x)

pattern Yield :: (Thread :>> sig) => Prog sig a -> Prog sig a
pattern Yield p <- (project -> (Just(Yield' p)))

yield :: (Thread :>> sig) => Prog sig ()
yield = inject (Yield' (pure ()))

pattern Fork :: (Thread :>> sig) => Prog sig x -> Prog sig a -> Prog sig a
pattern Fork child parent <- (project -> Just (Fork' child parent))

fork :: (Thread :>> sig) => Prog sig a -> Prog sig ()
fork child = inject (Fork' child (pure ()))

data SThread sig r =
    SYield (Prog (Thread:+: sig) r)
    | SFork (Daemon sig) (Prog (Thread :+: sig) r)
    | SActive r

instance (Syntax sig) => Functor (SThread sig) where
    fmap f = \case
        (SYield p) -> SYield (fmap f p)
        (SFork ch par) -> SFork ch (fmap f par)
        (SActive r) -> SActive (f r)

runThread :: (Syntax sig) => Prog (Thread :+: sig) a -> Prog sig (SThread sig a)
runThread = \case
    (Return x) -> pure (SActive x)
    (Yield p) -> pure (SYield p)
    (Fork ch par) -> pure (SFork (Daemon ch) par)
    (Other op) -> Op $ weave (SActive ()) thread op
    _ -> undefined

thread :: (Syntax sig) =>
    Handler (SThread sig) (Prog (Thread :+: sig)) (Prog sig)
thread = \case
    (SActive p) -> runThread p
    (SFork ch par) -> pure (SFork ch (join par))
    (SYield p) -> pure (SYield (join p))

schedule :: (Syntax sig) => Prog (Thread :+: sig) a -> Prog sig a
schedule p = master p [] where
    master p ds = do
        t <- runThread p
        case t of
            SActive x -> pure x
            SFork d p -> daemons (d:ds) [] p
            SYield p -> daemons [] ds p
    daemons [] ds' p = master p (reverse ds')
    daemons (Daemon q : ds) ds' p = do
        th <- runThread q
        case th of
            SActive _  -> daemons ds     ds'               p
            SFork d p' -> daemons (d:ds) (Daemon p' : ds') p
            SYield p'  -> daemons ds     (Daemon p' : ds') p