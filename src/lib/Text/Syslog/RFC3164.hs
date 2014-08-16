{-# LANGUAGE OverloadedStrings #-}

-- Specification: http://www.ietf.org/rfc/rfc3164.txt

module Text.Syslog.RFC3164
    ( Event(..)
    , Priority(..)
    , Header(..)
    , Content(..)
    , readEvent
    , showEvent
    ) where

import Text.Syslog.Types

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Time
import System.Locale

data Event = Event
    { priority :: Priority
    , header :: Header
    , content :: Content
    } deriving (Show, Read, Eq)

data Priority = Priority
    { facility :: Facility
    , severity :: Severity
    } deriving (Show, Read, Eq)

data Header = Header
    { timestamp :: UTCTime
    , host :: B.ByteString
    } deriving (Show, Read, Eq)

data Content = Content
    { tag :: Maybe B.ByteString
    , pid :: Maybe Int
    , message :: B.ByteString
    } deriving (Show, Read, Eq)

instance SyslogEvent Event where
    readEvent src = either (\_ -> Nothing) Just ee
        where ee = parseOnly parseEvent src
    showEvent e = foldl1 B.append [pri, hdr, cnt]
        where pri = priorityStr (priority e)
              hdr = headerStr (header e)
              cnt = contentStr (content e)

parseEvent :: Parser Event
parseEvent = Event <$> parsePriority <*> parseHeader <*> parseContent

parsePriority :: Parser Priority
parsePriority = do
  pri <- char '<' *> decimal <* char '>'
  return $ Priority (readFacility pri) (readSeverity pri)

parseHeader :: Parser Header
parseHeader = do
  time  <- parseTimestamp
  host' <- char ' ' *> takeTill (== ' ')
  return $ Header time host'

parseContent :: Parser Content
parseContent = do
  tag' <- Just <$> (char ' ' *> takeTill (`elem` ":[")) <|> pure Nothing
  pid' <- Just <$> (char '[' *> decimal <* string "]:") <|> (char ':' *> pure Nothing)
  msg  <- takeByteString
  return $ Content tag' pid' msg

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  time <- count 15 anyChar
  let time' = parseTime defaultTimeLocale timestampFormat time
  maybe (fail "invalid timestamp") return time'

readFacility :: Int -> Facility
readFacility pri = toEnum $ pri `div` 8

readSeverity :: Int -> Severity
readSeverity pri = toEnum $ pri `mod` 8

priorityStr :: Priority -> B.ByteString
priorityStr p = foldl1 B.append ["<", B.pack $ show (f + s), ">"]
    where f = fromEnum (facility p) * 8
          s = fromEnum (severity p)

headerStr :: Header -> B.ByteString
headerStr h = foldl1 B.append [B.pack $ timestampStr $ timestamp h, " ", host h, " "]

contentStr :: Content -> B.ByteString
contentStr c = foldl1 B.append [t, p, ":", message c]
    where t = maybe B.empty id (tag c)
          p = maybe B.empty (\p' -> foldl1 B.append ["[", B.pack $ show p', "]"]) (pid c)

timestampFormat :: String
timestampFormat = "%b %e %H:%M:%S"

timestampStr :: UTCTime -> String
timestampStr t = formatTime defaultTimeLocale timestampFormat t
