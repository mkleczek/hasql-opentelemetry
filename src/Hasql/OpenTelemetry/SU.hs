{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hasql.OpenTelemetry.Su where

import Hasql.Private.Session
import UnliftIO

deriving instance MonadUnliftIO Session