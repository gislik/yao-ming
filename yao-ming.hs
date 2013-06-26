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
import Data.Maybe           (listToMaybe)
import Data.Text            (Text, unwords, pack, null, unlines, append, strip)
import Data.Text.IO         (hGetLine, hPutStrLn, putStrLn)
import System.IO            (Handle, hFlush, hClose)
import Prelude hiding       (putStrLn, putStr, unwords, null, unlines)

-- configuration
defaultPort :: PortNumber
defaultPort = 8080

programName :: Text
programName = "yao-ming"

programVersion :: Text
programVersion = "0.0.2"

-- main
main :: IO ()
main = withSocketsDo $ do
   putStrLn $ unwords ["Starting", programName,  "version", programVersion, "on port", pack (show defaultPort)]
   {- putStrLn $ unwords [programName, programVersion] --, show defaultPort] -}
   installHandler sigPIPE Ignore Nothing
   url <- maybe "http://example.com/" pack <$> listToMaybe <$> getArgs
   bracket 
      (acquireSocket $ PortNumber defaultPort)
      closeSocket
      (flip acceptConnection $ redirectConnection url)

redirectConnection :: Text -> Handle -> IO ()
redirectConnection url h = readRequest h >> hPutStrLn h (constructResponse url) >> hFlush h >> hClose h
   
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
