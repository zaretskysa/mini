
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances   #-}

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error
import Data.Function
import Data.IORef

handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

test2 :: ErrorT String (ContT () IO) ()
test2 = handleError (\e -> throwError (e ++ ":top")) $ do
    x <- liftIO $ newIORef 1
    label <- callCC (return . fix)
    v <- liftIO (readIORef x)
    liftIO (print v)
    handleError (\e -> throwError (e ++ ":middle")) $ do
        when (v==4) $ do
            throwError "ouch"
    when (v < 10) $ do
        liftIO (writeIORef x (succ v))
        handleError (\e -> throwError (e ++ ":" ++ show v)) label
    liftIO $ print "done"

go2 = runContT (runErrorT test2) (either error return)
{-

*Main> go2
1
2
3
4
*** Exception: ouch:middle:top

-}


instance MonadError e m => MonadError e (ContT r m) where
  throwError = lift . throwError
  catchError op h = ContT $ \k -> catchError (runContT op k) (\e -> runContT (h e) k)

test3 :: ContT () (ErrorT String IO) ()
test3 = handleError (\e -> throwError (e ++ ":top")) $ do
  x <- liftIO $ newIORef 1
  label <- callCC (return . fix)
  v <- liftIO (readIORef x)
  liftIO (print v)
  handleError (\e -> throwError (e ++ ":middle")) $ do
    when (v==4) $ do
      throwError "ouch"
  when (v < 10) $ do
         liftIO (writeIORef x (succ v))
         handleError (\e -> throwError (e ++ ":" ++ show v)) label
  liftIO $ print "done"

go3 = runErrorT (runContT test3 return)

{-

*Main> go3
1
2
3
4
Left "ouch:middle:3:middle:2:middle:1:middle:top"

-}

