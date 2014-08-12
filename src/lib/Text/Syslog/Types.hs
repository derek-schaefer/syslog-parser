module Text.Syslog.Types where

import qualified Data.ByteString.Char8 as B

class SyslogEvent e where
    readEvent :: B.ByteString -> Maybe e
    showEvent :: e -> B.ByteString

data Severity = Emergency
              | Alert
              | Critical
              | Error
              | Warning
              | Notice
              | Info
              | Debug
                deriving (Show, Read, Eq, Enum, Ord, Bounded)

data Facility = Kern
              | User
              | Mail
              | Daemon
              | Auth
              | Internal
              | Lpr
              | News
              | Uucp
              | Clock
              | AuthPriv
              | Ftp
              | Ntp
              | LogAudit
              | LogAlert
              | Cron
              | Local0
              | Local1
              | Local2
              | Local3
              | Local4
              | Local5
              | Local6
              | Local7
                deriving (Show, Read, Eq, Enum, Ord, Bounded)
