{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Vector.Machine where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Machine
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

buffered :: forall m a. MonadIO m => Int -> ProcessT m a (V.Vector a)
buffered c = repeatedly $ do
  v <- liftIO $ M.new c
  build v 0
  where
    end = c
    build v i = if i == end
      then liftIO (V.unsafeFreeze v) >>= \fv -> yield fv
      else do
        x <- await <|> do
          fv <- liftIO $ V.unsafeFreeze $ M.unsafeSlice 0 (i - 1) v
          yield fv
          stop
        liftIO $ M.write v i x
        build v $! i + 1
{-# INLINEABLE buffered #-}
