
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
import Data.Time
import Data.Char
import qualified Data.Map as DataMap
import Data.List
import Control.Exception hiding (try)
import qualified Control.Exception as Ex

data InternalServerState 
   = InternalServerState
     { isLogEnabled    :: Bool
     , sock            :: Maybe Socket
     , pool            :: Maybe (MVar WorkerPool)  }

type Environment = Int

defaultInternalServerState = InternalServerState { isLogEnabled = False, sock = Nothing, pool = Nothing }

main = do
    (port:_) <- getArgs
    sck <- listenOn (PortNumber (fromIntegral (read port :: Int)))
    workerPoolMVar <- newMVar $ WorkerPool 0 [] []
    let iss =  defaultInternalServerState { sock = Just sck, pool = Just workerPoolMVar }
    putStrLn $ "server is starting on port: " ++ show port
    runServer iss


runServer :: InternalServerState -> IO ()
runServer iss@(InternalServerState isLogEnabled (Just socket) (Just workerPoolMVar)) =  Ex.bracket setup exit loop
    where  setup = do putStrLn "ready for requests..."
                      return (socket, workerPoolMVar)

           loop (s, workerPoolMVar)  = do (sock', sockAddr) <- accept s
                                          WorkerThread _ chan <- getWorkerThread workerPoolMVar 0
                                          writeChan chan sock'
                                          loop (s, workerPoolMVar)

           exit (s, _) = do putStrLn "Exiting..."
                            sClose s


------------------------------------------------
-- | Worker stuff
------------------------------------------------

workerLoop :: MVar WorkerPool ->
              Environment     ->
              Chan Socket     ->
              IO ()
workerLoop workerPoolMVar e chan
    = do mainLoop e
    where
      mainLoop e
          = do sock      <- readChan chan
               work sock e
               putWorkerThread workerPoolMVar chan
               mainLoop (e + 1)


work :: Socket -> Environment -> IO ()
work sock' e  = do
    r <- getRequest sock'
    case r of
          Just req@(Request uri method headers body) -> do putStrLn "request received"
                                                           sendResponse sock' $ buildResponse uri e
          Nothing -> putStrLn "error caught..."
    sClose sock'

buildResponse :: URI -> Environment -> String
buildResponse uri@(URI scheme _ path query fragment) e = "counter[ " ++ show e ++ "] <br><br> " ++
                                                         "You have requested-- scheme[" ++ scheme ++ "] path [" ++ path ++ "] query [" ++ query ++ "] fragment ["++ fragment ++ "]"




------------------------------------------------
-- | Worker Pool stuff
------------------------------------------------

type ExpiresTime = UTCTime
data WorkerThread = WorkerThread ThreadId (Chan Socket)
data WorkerPool = WorkerPool { numWorkers :: Int,
                               idleWorkers :: [WorkerThread],
                               busyWorkers :: [(WorkerThread, ExpiresTime)]}

--getWorkerThread :: MVar WorkerPool -> IO WorkerThread
getWorkerThread mv e = 
  do wp <- takeMVar mv
     case wp of
       WorkerPool n [] bs -> 
         do chan <- newChan
--            e' <- runController (addDatabaseToEnvironment) e
--            tid <- forkIO $ workerLoop mv e' chan
            tid <- forkIO $ workerLoop mv e chan
            let workerThread = WorkerThread tid chan 
            expiresTime <- getCurrentTime >>= \utct -> return $ addUTCTime (fromInteger stdTimeOut) utct
            putMVar mv $ WorkerPool (n+1) [] ((workerThread, expiresTime):bs)
            return workerThread
       WorkerPool n (idle:idles) busies ->
         do expiresTime <- getCurrentTime >>= \utct -> return $ addUTCTime (fromInteger stdTimeOut) utct
            putMVar mv $ WorkerPool n idles ((idle, expiresTime):busies)
            return idle

putWorkerThread mv chan = do
               WorkerPool n is bs <- takeMVar mv
               mytid <- myThreadId
               let bs' = filter (\(WorkerThread tid _, _) -> tid /= mytid) bs
               putMVar mv $ WorkerPool n ((WorkerThread mytid chan):is) bs'


{--
timeout :: Int -> ThreadId -> IO ()
timeout time thid
    = do threadDelay time
         throwTurbinadoTo thid TimedOut
--}

-- conf. files? Indeed! haha
stdTimeOut :: Integer
stdTimeOut = 90



------------------------------------------------
-- | HTTP Stuff
------------------------------------------------

getRequest :: Socket  -> IO (Maybe Request)
getRequest sock = do
    req <- receiveHTTP sock
    case req of
          Left _ -> return Nothing
          Right r -> return $ Just r

sendResponse :: Socket -> String -> IO ()
sendResponse sock xs = do t <- getClockTime
                          let resp = Response (2,0,0) "OK" (buildHeaders (Just $ length xs) t []) (xs)
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
