module Text.Syslog.Types where

import Data.Text (Text)

class SyslogEvent e where
    readEvent :: Text -> Maybe e
    showEvent :: e    -> Text

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
              | Auth0
              | Internal
              | Lpr
              | News
              | Uucp
              | Cron
              | Auth1
              | Ftp
              | Ntp
              | LogAudit
              | LogAlert
              | Clock
              | Local0
              | Local1
              | Local2
              | Local3
              | Local4
              | Local5
              | Local6
              | Local7
                deriving (Show, Read, Eq, Enum, Ord, Bounded)
