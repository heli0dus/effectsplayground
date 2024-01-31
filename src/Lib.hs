module Lib (mainDiv) where

import Control.Effect.State
import Control.Effect.Error
import Control.Effect.Reader
import Control.Effect.Lift
import Control.Carrier.State.Strict
import Control.Carrier.Reader
import Control.Carrier.Error.Either (runError)
import Control.Monad.IO.Class
import System.Environment
import Text.Read (readMaybe)
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

example3 :: (Has (State Int) sig m) => m (Int, ())
example3 =  runState 0 . runReader "hello" $ do
  list <- ask
  put (length (list :: String))

safeHead :: (Has (Error String) sig m, Read a) => [String] -> m a
safeHead [] = throwError "Not enough arguments provided"
safeHead (x:_) = case readMaybe x of
  Nothing -> throwError "Expeced int as argument, got something else"
  Just x -> pure x

getNInts ::(Has (Error String) sig m, MonadIO m) => Int -> m [Int]
getNInts x = forM [1..x] \line -> do
  a <- liftIO getLine
  case readMaybe a of
    Just x -> pure x
    Nothing -> throwError $
       "Expected number but got " <> a <> " at line " <> show line

safeDiv :: (Has (Error String) sig m) => Int -> Int -> m Int
safeDiv a b = case b of --todo "safeDiv"
  0 -> throwError "Division by zero!"
  _ -> pure (a `div` b)

safeDivFold :: (Has (Error String) sig m) => [Int] -> m Int
safeDivFold = foldM safeDiv 100500

divArgs :: IO (Either String Int)
divArgs = runError do
    inp <- liftIO getArgs
    n <- safeHead inp `catchError` \e -> throwError $ "CLI Error: " <> e
    xs<- getNInts n `catchError` \e -> throwError $ "Input error: " <> e
    safeDivFold xs `catchError` \e -> throwError $ "Div error: " <> e

mainDiv :: IO ()
mainDiv = do
  x <- runError @String do
    inp <- liftIO getArgs
    n <- safeHead inp `catchError` \e -> throwError $ "CLI Error: " <> e
    xs<- getNInts n `catchError` \e -> throwError $ "Input error: " <> e
    safeDivFold xs `catchError` \e -> throwError $ "Div error: " <> e
  either print print x

-- | 4. (2б)
-- Реализуйте интерфейс, позволяющий кидать и ловить ошибки в языке,
-- который предоставляет ваш трансформер.

-- getNInts :: Int -> EitherT String IO [Int]
-- getNInts x = forM [1..x] \line -> do
--   a <- lift getLine
--   case readMaybe a of
--     Just x -> pure x
--     Nothing -> raiseError $
--        "Expected number but got " <> a <> " at line " <> show line

-- instance Monad m => MonadError e (EitherT e m) where
--   throwError = raiseError --todo "EitherT throwError"
--   catchError m k = EitherT $ do--todo "EitherT catchError"
--     x <- runEitherT m
--     case x of
--       Right x -> pure (Right x)
--       Left e  -> runEitherT $ k e

-- Реализуйте приложение, которое с помощью getNInts читает столько чисел с
-- консоли, сколько передали в первом аргументе командной строки, и делит 100500
-- на все переданные числа 100500 `div` x1 `div` x2 `div` ... с помощью safeDiv.
-- Если произошла неудача, напечатайте вместо результата деления сообщение об
-- ошибке с одним из следующих префиксов: "CLI error:", "Input error:", "Div error:".
-- Протестируйте с помощью `stack run -- N` или `ghci> :run mainDiv N`

-- safeDivFold :: Monad m => [Int] -> EitherT String m Int
-- safeDivFold = foldM safeDiv 100500

-- safeHead:: (Read a, Monad m) => [String] -> EitherT String m a
-- safeHead [] = throwError "Not enough arguments provided"
-- safeHead (x:_) = case readMaybe x of
--   Nothing -> throwError "Expeced int as argument, got something else"
--   Just x -> pure x

-- mainDiv :: IO ()
-- mainDiv = do
--   x <- runEitherT do
--     inp <- lift getArgs
--     -- Старую версию оставлю из соображений понять так нормально или лучше делать таки доп функцию
--     -- when (null a) $ throwError "CLI Error: not enough arguments provided"
--     -- let x' = readMaybe $ head a
--     -- x <- maybe (throwError"CLI Error: expeced int, got something else") pure x'
--     n <- safeHead inp `catchError` \e -> throwError $ "CLI Error: " <> e
--     xs<- getNInts n `catchError` \e -> throwError $ "Input error: " <> e
--     safeDivFold xs `catchError` \e -> throwError $ "Div error: " <> e
--   either print print x

example4 :: (Has (State String) sig m, Has (State Integer) sig m) => m ()
example4 = do
  modify (+1)
  modify (++ "Jimmy")

example5 :: (Int, (String, ()))
example5 = run . runState @Int (0::Int) . runState @String "hello, " $ do
  put (1::Int)
  modify (++ "Jimmy")