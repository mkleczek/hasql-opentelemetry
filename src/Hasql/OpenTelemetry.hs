module Hasql.OpenTelemetry (
  Traced,
  ToAttr (..),
  ToAttributes (..),
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Kind (Type)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Hasql.Api qualified as Api
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.Trace.Core (SpanArguments (..), SpanKind (Client))
import OpenTelemetry.Trace.Monad (MonadTracer (getTracer), inSpan)
import UnliftIO (MonadIO, MonadUnliftIO)

newtype Traced s m a = Traced (ReaderT [(Text, Attribute)] m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadUnliftIO)

instance (MonadTracer m) => MonadTracer (Traced s m) where
  getTracer = lift getTracer

class ToAttr (s :: Type -> Type -> Type) where
  toAttr :: s x y -> Attribute

inSpanQuery :: (MonadUnliftIO m, MonadTracer m) => [(Text, Attribute)] -> Attribute -> m a -> m a
inSpanQuery attrs stmt = inSpan "pg.query" (SpanArguments Client (("db.statement", stmt) : attrs) [] Nothing)

instance (ToAttr s, MonadUnliftIO m, Api.Sql s m, MonadTracer m) => Api.Sql s (Traced s m) where
  sql query = Traced $ do
    attrs <- ask
    lift $ inSpanQuery attrs (toAttribute $ decodeUtf8 query) (Api.sql query)
  statement parameters s = Traced $ do
    attrs <- ask
    lift $ inSpanQuery attrs (toAttr s) (Api.statement parameters s)

class ToAttributes m a where
  toAttributes :: a -> m [(Text, Attribute)]

instance (Api.Sql s m, Api.RunnableSql m c e, MonadTracer m, ToAttributes m c, ToAttributes IO c) => Api.RunnableSql (Traced s m) c e where
  run (Traced r) connection = do
    attrs <- toAttributes connection
    Api.run (runReaderT r attrs) connection
