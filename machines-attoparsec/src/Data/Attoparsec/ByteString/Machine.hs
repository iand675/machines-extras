{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Attoparsec.ByteString.Machine where
import Control.Exception
import Control.Monad.Trans
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Machine
import Data.Machine.Stack
import Data.Typeable

data ParseError = ParseError [String] String
  deriving (Show, Typeable)

instance Exception ParseError

-- Throws a ParseError in the event that a parse fails
parsed :: Parser a -> Process ByteString (Either ParseError a)
parsed p = stack echo $ repeatedly (pop >>= go . parse p)
  where
    go (Done i x) = push i >> yield (Right x)
    go (Partial f) = pop >>= go . f 
    go (Fail i ctxts msg) = push i >> yield (Left $ ParseError ctxts msg)
{-# INLINEABLE parsed #-}

