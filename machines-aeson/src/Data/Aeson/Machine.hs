{-# LANGUAGE RankNTypes #-}
module Data.Aeson.Machine where
import Control.Exception
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Parser
import Data.ByteString (ByteString)
import Data.Machine
import Data.Typeable
import qualified Data.Attoparsec.ByteString.Machine as P
import qualified Data.ByteString.Lazy.Internal as L

data JsonError
  = ParseError [String] String
  | InvalidStructureError Value String
  deriving (Show, Typeable)

instance Exception JsonError 

-- | Parses a stream of JSON values (objects or arrays)
decoded :: forall a. (FromJSON a) => Process ByteString (Either JsonError a)
decoded = P.parsed json ~> mapping convert
  where
    convert (Left (P.ParseError ss e)) = Left $ ParseError ss e
    convert (Right val) = case fromJSON val of
      Error e -> Left $ InvalidStructureError val e
      Success x -> Right x


-- | Parses a stream of JSON values (including non-array, non-object values)
decoded' :: (FromJSON a) => Process ByteString (Either JsonError a)
decoded' = P.parsed value ~> mapping convert
  where
    convert (Left (P.ParseError ss e)) = Left $ ParseError ss e
    convert (Right val) = case fromJSON val of
      Error e -> Left $ InvalidStructureError val e
      Success x -> Right x

class Encoded o where
  encoded :: ToJSON a => Process a o

instance Encoded ByteString where
  encoded = repeatedly $ do
    x <- await
    go $ encode x
    where
      go (L.Chunk c bs) = yield c >> go bs
      go (L.Empty) = return ()

instance Encoded Value where
  encoded = repeatedly $ do
    x <- await
    yield $ toJSON x


