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
import Control.Exception    (bracket)
import System.Posix         (Handler(Ignore), installHandler, sigPIPE)
import System.Environment   (getArgs)
import Data.Maybe           (fromMaybe, listToMaybe)
import System.IO            (Handle, hPutStrLn, hFlush, hClose)

-- configuration
defaultPort :: PortNumber
defaultPort = 8080

programName :: String
programName = "yao-ming"

programVersion :: String
programVersion = "0.0.1"

-- main
main :: IO ()
main = withSocketsDo $ do
   putStrLn $ unwords ["Starting", programName,  "version", programVersion, "on port", show defaultPort]
   installHandler sigPIPE Ignore Nothing
   url <- fromMaybe "http://example.com/" <$> listToMaybe <$> getArgs
   bracket 
      (acquireSocket $ PortNumber defaultPort)
      closeSocket
      (flip acceptConnection $ redirectConnection url)

redirectConnection :: String -> Handle -> IO ()
redirectConnection url h = hPutStrLn h (constructResponse url) >> hFlush h >> hClose h

-- helpers
constructResponse :: String -> String
constructResponse url = unlines ["HTTP/1.1 301 Moved Permanently"
                                ,"Location: " ++ url
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
