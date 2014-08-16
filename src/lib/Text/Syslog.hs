module Text.Syslog
    ( SyslogEvent(..)
    , Facility(..)
    , Severity(..)
    , readRFC3164
    ) where

import Text.Syslog.Types
import qualified Text.Syslog.RFC3164 as RFC3164

import qualified Data.ByteString.Char8 as B

readRFC3164 :: B.ByteString -> Maybe RFC3164.Event
readRFC3164 src = readEvent src
