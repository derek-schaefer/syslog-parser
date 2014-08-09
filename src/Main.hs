module Main where

import Text.Syslog.Types
import Text.Syslog.RFC3164 as RFC3164

import qualified Data.Text as T
import System.Environment

main :: IO ()
main = parseLines readRFC3164

readRFC3164 :: String -> Maybe RFC3164.Event
readRFC3164 src = RFC3164.readEvent $ T.pack src

parseLines :: (SyslogEvent e, Show e) => (String -> Maybe e) -> IO ()
parseLines reader = do
  src <- getLine
  case src of
    [] -> return ()
    _  -> do
      case (reader src) of
        Just e -> do
             putStrLn $ show e
             putStrLn $ show $ showEvent e
             putStrLn "\n"
      parseLines reader
