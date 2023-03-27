module Hasql.OpenTelemetry (
  Traced,
  ToAttr (..),
  ToAttributes (..),
) where

import Control.Exception.Safe (MonadMask, SomeException (SomeException), bracketWithError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Kind (Type)
import Data.Text
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack, SrcLoc (srcLocFile, srcLocModule, srcLocPackage, srcLocStartLine), callStack, getCallStack)
import Hasql.Api qualified as Api
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.Context (insertSpan, lookupSpan)
import OpenTelemetry.Context.ThreadLocal (adjustContext, getContext)
import OpenTelemetry.Trace.Core (SpanArguments (..), SpanKind (Client), addAttributes, createSpanWithoutCallStack, endSpan, whenSpanIsRecording)
import OpenTelemetry.Trace.Monad (MonadTracer (getTracer))

newtype Traced s m a = Traced (ReaderT [(Text, Attribute)] m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadTracer m) => MonadTracer (Traced s m) where
  getTracer = lift getTracer

class ToAttr (s :: Type -> Type -> Type) where
  toAttr :: s x y -> Attribute

inSpan :: (MonadIO m, HasCallStack, MonadTracer m, MonadMask m) => Text -> SpanArguments -> m a -> m a
inSpan n args f = do
  t <- getTracer
  bracketWithError
    ( do
        let cs = callStack
        ctx <- getContext
        s <- createSpanWithoutCallStack t ctx n args
        adjustContext (insertSpan s)
        whenSpanIsRecording s $ do
          case getCallStack cs of
            [] -> pure ()
            (fn, loc) : _ -> do
              addAttributes
                s
                [ ("code.function", toAttribute $ T.pack fn)
                , ("code.namespace", toAttribute $ T.pack $ srcLocModule loc)
                , ("code.filepath", toAttribute $ T.pack $ srcLocFile loc)
                , ("code.lineno", toAttribute $ srcLocStartLine loc)
                , ("code.package", toAttribute $ T.pack $ srcLocPackage loc)
                ]
        pure (lookupSpan ctx, s)
    )
    ( \e (parent, child) -> do
        case e of
          Just (SomeException inner) -> pure ()
          Nothing -> pure ()
        endSpan child Nothing
    )
    (const f)

inSpanQuery :: (MonadIO m, MonadMask m, MonadTracer m) => [(Text, Attribute)] -> Attribute -> m a -> m a
inSpanQuery attrs stmt = inSpan "pg.query" (SpanArguments Client (("db.statement", stmt) : attrs) [] Nothing)

instance (ToAttr s, Api.Sql s m, MonadTracer m, MonadIO m, MonadMask m) => Api.Sql s (Traced s m) where
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

-- inSpan'' ::
--   (MonadIO m, HasCallStack) =>
--   Tracer ->
--   -- | Record the location of the span in the codebase using the provided
--   -- callstack for source location info.
--   CallStack ->
--   -- | The name of the span. This may be updated later via 'updateName'
--   Text ->
--   SpanArguments ->
--   (Span -> m a) ->
--   m a
-- inSpan'' t cs n args f = do
--   liftIO $
--     bracket_
--       ( liftIO $ do
--           ctx <- getContext
--           s <- createSpanWithoutCallStack t ctx n args
--           adjustContext (insertSpan s)
-- whenSpanIsRecording s $ do
--   case getCallStack cs of
--     [] -> pure ()
--     (fn, loc) : _ -> do
--       OpenTelemetry.Trace.Core.addAttributes
--         s
--         [ ("code.function", toAttribute $ T.pack fn)
--         , ("code.namespace", toAttribute $ T.pack $ srcLocModule loc)
--         , ("code.filepath", toAttribute $ T.pack $ srcLocFile loc)
--         , ("code.lineno", toAttribute $ srcLocStartLine loc)
--         , ("code.package", toAttribute $ T.pack $ srcLocPackage loc)
--         ]
-- pure (lookupSpan ctx, s)
--       )
--       ( \e (parent, s) -> do
-- forM_ e $ \(SomeException inner) -> do
--   setStatus s $ Error $ T.pack $ displayException inner
--   recordException s [] Nothing inner
-- endSpan s Nothing
-- adjustContext $ \ctx ->
--   maybe (removeSpan ctx) (`insertSpan` ctx) parent
--       )
--       (\(_, s) -> f s)
