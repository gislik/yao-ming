{-# LANGUAGE OverloadedStrings #-}

module Main where

{- Yao Ming is a simple HTTP rebounder which sends a HTTP 301 response no matter what the request was:
 
 HTTP/1.1 301 Moved Permanently
 Location: <url>
 Content-Length: 0
  
 -} 

import Network              (PortID(..), PortNumber, withSocketsDo, listenOn, accept)
import Network.Socket       (Socket, close)
import Control.Concurrent   (forkIO)
import Control.Applicative  ((<$>))
import Control.Monad        (unless)
import Control.Exception    (bracket)
import System.Posix         (Handler(Ignore), installHandler, sigPIPE)
import System.Environment   (getArgs)
import System.IO            (Handle, hFlush, hClose)
import System.Timeout       (timeout)
import Data.Maybe           (listToMaybe)
import Data.Text            (Text, unwords, pack, null, unlines, append, strip)
import Data.Text.IO         (hGetLine, hPutStrLn, putStrLn)
import Prelude hiding       (putStrLn, putStr, unwords, null, unlines)

-- configuration
defaultPort :: PortNumber
defaultPort = 8080

defaultTimeout :: Int
defaultTimeout = 10000000

programName :: Text
programName = "yao-ming"

programVersion :: Text
programVersion = "0.2.0"

-- main
main :: IO ()
main = withSocketsDo $ do
   putStrLn $ unwords ["Starting", programName,  "version", programVersion, "on port", pack (show defaultPort)]
   installHandler sigPIPE Ignore Nothing
   url <- maybe "http://example.com/" pack <$> listToMaybe <$> getArgs
   bracket 
      (acquireSocket $ PortNumber defaultPort)
      closeSocket
      (flip acceptConnection $ redirectConnection url)

redirectConnection :: Text -> Handle -> IO ()
redirectConnection url h = timeout defaultTimeout (readRequest h) >>= sendRedirect >> hClose h   
   where
      sendRedirect (Just _) = sendResponse h (constructResponse url)
      sendRedirect Nothing  = return ()

sendResponse :: Handle -> Text -> IO ()
sendResponse h r = hPutStrLn h r >> hFlush h

readRequest :: Handle -> IO ()
readRequest h = do
   line <- strip <$> hGetLine h 
   unless (null line) (readRequest h)

-- helpers
constructResponse :: Text -> Text
constructResponse url = unlines ["HTTP/1.1 301 Moved Permanently"
                                ,append "Location: " url
                                ,"Content-Length: 0"]

acceptConnection :: Socket -> (Handle -> IO ()) -> IO ()
acceptConnection socket handler = do
   (h,_,_) <- accept socket
   forkIO (handler h)
   acceptConnection socket handler

acquireSocket :: PortID -> IO Socket
acquireSocket = listenOn 

closeSocket :: Socket -> IO ()
closeSocket = close
