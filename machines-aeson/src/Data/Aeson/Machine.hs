module Data.Aeson.Machine where
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Parser
import Data.ByteString (ByteString)
import Data.Machine
import qualified Data.Machine.Attoparsec.ByteString as P
import qualified Data.ByteString.Lazy.Internal as L

data JsonError
  = ParseError [String] String
  | InvalidStructureError Value String
  deriving (Show, Typeable)

instance Exception JsonError 

rethrowParseError :: (MonadThrow m) => P.ParseError -> ProcessT m k a
rethrowParseError (P.ParseError ss e) = lift $ throwM $ ParseError ss e

-- | Parses a stream of JSON values (objects or arrays)
decoded :: (MonadThrow m, FromJSON a) => ProcessT m ByteString a
decoded = catch (P.parsed json ~> repeatedly convert) rethrowParseError
  where
    convert = do
      val <- await
      case fromJSON val of
        Error e -> lift $ throwM $ InvalidStructureError val e
        Success x -> yield x

-- | Parses a stream of JSON values (including non-array, non-object values)
decoded' :: (MonadThrow m, FromJSON a) => ProcessT m ByteString a
decoded' = catch (P.parsed value ~> repeatedly convert) rethrowParseError
  where
    convert = do
      val <- await
      case fromJSON val of
        Error e -> lift $ throwM $ InvalidStructureError val e
        Success x -> yield x

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


