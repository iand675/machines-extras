{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Attoparsec.Text.Machine where
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Machine
import Data.Machine.Stack
import Data.Typeable

data ParseError = ParseError [String] String
  deriving (Show, Typeable)

instance Exception ParseError

-- Throws a ParseError in the event that a parse fails
parsed :: MonadThrow m => Parser a -> ProcessT m Text a
parsed p = stack echo $ repeatedly (pop >>= go . parse p)
  where
    go (Done i x) = push i >> yield x
    go (Partial f) = pop >>= go . f
    go (Fail i ctxts msg) = push i >> lift (throwM $ ParseError ctxts msg)
{-# INLINEABLE parsed #-}

