module Main where

import Text.Log.Syslog

import qualified Data.ByteString.Char8 as B

main :: IO ()
main = parseLines (\src -> readRFC3164 $ B.pack src)

parseLines :: (SyslogEvent e, Show e) => (String -> Maybe e) -> IO ()
parseLines reader = do
  src <- getLine
  case src of
    [] -> return ()
    _  -> do
      case (reader src) of
        Nothing -> putStrLn ""
        Just e  -> do
             putStrLn $ show e
             putStrLn $ show $ showEvent e
             putStrLn ""
      parseLines reader
