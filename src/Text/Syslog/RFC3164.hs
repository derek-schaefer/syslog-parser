{-# LANGUAGE OverloadedStrings #-}

-- Specification: http://www.ietf.org/rfc/rfc3164.txt

module Text.Syslog.RFC3164
    ( Event(..), Priority(..), Header(..), Content(..)
    , readEvent, showEvent
    ) where

import Text.Syslog.Types

import Control.Applicative
import Data.Attoparsec.Text
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
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
    , host :: T.Text
    } deriving (Show, Read, Eq)

data Content = Content
    { tag :: Maybe T.Text
    , pid :: Maybe Int
    , message :: T.Text
    } deriving (Show, Read, Eq)

instance SyslogEvent Event where
    readEvent src = either (\_ -> Nothing) Just ee
        where ee = parseOnly parseEvent src
    showEvent e = foldl1 T.append [pri, hdr, cnt]
        where pri = priorityStr (priority e)
              hdr = headerStr (header e)
              cnt = contentStr (content e)

parseEvent :: Parser Event
parseEvent = do
  pri <- parsePriority
  hdr <- parseHeader
  cnt <- parseContent
  return $ Event pri hdr cnt

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
  msg  <- takeText
  return $ Content tag' pid' msg

timestampFormat :: String
timestampFormat = "%b %e %H:%M:%S"

parseTimestamp :: Parser UTCTime
parseTimestamp = do
  time <- count 15 anyChar
  return $ fromJust $ parseTime defaultTimeLocale timestampFormat time

readFacility :: Int -> Facility
readFacility pri = toEnum $ pri `div` 8

readSeverity :: Int -> Severity
readSeverity pri = toEnum $ pri `mod` 8

priorityStr :: Priority -> T.Text
priorityStr p = foldl1 T.append ["<", T.pack $ show (f + s), ">"]
    where f = fromEnum (facility p) * 8
          s = fromEnum (severity p)

headerStr :: Header -> T.Text
headerStr h = foldl1 T.append [T.pack $ timestampStr $ timestamp h, " ", host h, " "]

contentStr :: Content -> T.Text
contentStr c = foldl1 T.append [t, p, ": ", message c]
    where t = maybe T.empty id (tag c)
          p = maybe T.empty (\p -> foldl1 T.append ["[", T.pack $ show p, "]"]) (pid c)

timestampStr :: UTCTime -> String
timestampStr t = formatTime defaultTimeLocale timestampFormat t
