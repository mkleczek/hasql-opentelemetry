module Hasql.OpenTelemetry.Session (
  Session,
) where

import Data.ByteString.Char8 qualified as C
import Data.IP (IP (..))
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Hasql.Connection (Connection, withLibPQConnection)
import Hasql.OpenTelemetry
import Hasql.Session qualified as S
import Hasql.Statement (Statement (..))
import OpenTelemetry.Attributes (ToAttribute (..))
import OpenTelemetry.Resource ((.=), (.=?))
import Text.Read (readMaybe)

type Session = Traced Statement S.Session

instance ToAttr Statement where
  toAttr (Statement q _ _ _) = toAttribute $ decodeUtf8 q

instance ToAttributes IO Connection where
  toAttributes c = do
    (mDb, mUser, mHost, mPort) <- withLibPQConnection c $ \pqConn -> do
      (,,,)
        <$> LibPQ.db pqConn
        <*> LibPQ.user pqConn
        <*> LibPQ.host pqConn
        <*> LibPQ.port pqConn
    pure $
      ("db.system", toAttribute ("postgresql" :: Text))
        : catMaybes
          [ "db.user" .=? (decodeUtf8 <$> mUser)
          , "db.name" .=? (decodeUtf8 <$> mDb)
          , "net.peer.port"
              .=? ( do
                      port <- decodeUtf8 <$> mPort
                      (readMaybe $ unpack port) :: Maybe Int
                  )
          , case (readMaybe . C.unpack) =<< mHost of
              Nothing -> "net.peer.name" .=? (decodeUtf8 <$> mHost)
              Just (IPv4 ip4) -> "net.peer.ip" .= pack (show ip4)
              Just (IPv6 ip6) -> "net.peer.ip" .= pack (show ip6)
          ]
