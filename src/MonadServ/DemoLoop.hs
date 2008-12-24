
module Main where

import Network hiding (accept)
import Network.Socket
import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import System.Environment
import Control.Concurrent
import System.IO
import System.Locale
import System.Time

import Data.Char
import qualified Data.Map as DataMap
import Data.List
import Control.Exception hiding (try)
import qualified Control.Exception as Ex

data InternalServerState 
   = InternalServerState
     { isLogEnabled    :: Bool
     , sock            :: Maybe Socket  }

defaultInternalServerState = InternalServerState { isLogEnabled = False, sock = Nothing }

main = do
    (port:_) <- getArgs
    sck <- listenOn (PortNumber (fromIntegral (read port :: Int)))
    let iss =  defaultInternalServerState { sock = Just sck }
    putStrLn $ "server is starting on port: " ++ show port
    runServer iss


runServer :: InternalServerState -> IO ()
runServer iss@(InternalServerState isLogEnabled (Just socket)) =  Ex.bracket setup exit loop
    where  setup = do putStrLn "ready for requests..."
                      return socket
           loop s  = do accept s >>= forkIO . work
                        loop s
           exit s = do putStrLn "Exiting..."
                       sClose s


work :: (Socket, SockAddr) -> IO ()
work (sock', sockAddr)  = do
    r <- getRequest sock'
    case r of
          Just req@(Request uri method headers body) -> do putStrLn "request received"
                                                           sendResponse sock' $ buildResponse uri
          Nothing -> putStrLn "error caught..."
    sClose sock'



buildResponse :: URI -> String
buildResponse uri@(URI scheme _ path query fragment) = "You have requested-- scheme[" ++ scheme ++ "] path [" ++ path ++ "] query [" ++ query ++ "] fragment ["++ fragment ++ "]"


getRequest :: Socket  -> IO (Maybe Request)
getRequest sock = do
    req <- receiveHTTP sock
    case req of
          Left _ -> return Nothing
          Right r -> return $ Just r

sendResponse :: Socket -> String -> IO ()
sendResponse sock xs = do t <- getClockTime
                          let resp = Response (2,0,0) "OK" (buildHeaders (Just $ length xs) t (startingHeaders t)) (xs)
                          respondHTTP sock resp


buildHeaders :: Maybe Int -> ClockTime -> [Header] -> [Header]
buildHeaders Nothing  t hdrs = union hdrs ( startingHeaders t)
buildHeaders (Just l) t hdrs = union hdrs ((startingHeaders t) ++
                                [Header HdrContentLength $ show l]) 

startingHeaders t = [ Header HdrServer "Jaxclipse  www.jaxclipse.com"
                    , Header HdrContentType "text/html; charset=UTF-8"
                    , Header HdrDate $ formatCalendarTime defaultTimeLocale rfc822DateFormat $ toUTCTime t
                    ]

instance Eq Header where
  (==) (Header hn1 _) (Header hn2 _) = hn1 == hn2
