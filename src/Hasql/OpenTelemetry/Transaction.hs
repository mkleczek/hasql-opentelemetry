module Hasql.OpenTelemetry.Transaction (
  Transaction,
) where

import Hasql.OpenTelemetry.Session (Session)
import Hasql.Transaction qualified

type Transaction = Hasql.Transaction.Transaction Session
