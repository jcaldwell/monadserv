
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
import Text.ParserCombinators.Parsec
import Numeric (readHex)
import Data.Map as Map hiding (filter, union)
import Control.Monad
import Data.Maybe

data InternalServerState 
   = InternalServerState
     { isLogEnabled    :: Bool
     , sock            :: Maybe Socket
     , pool            :: Maybe (MVar WorkerPool)  }

--type Environment = Int

data Environment = Environment { hitCounter :: Int
                               , store :: DataMap.Map String Int }
                   deriving (Show)

defaultInternalServerState = InternalServerState { isLogEnabled = False, sock = Nothing, pool = Nothing }

initialEnvironment = Environment { hitCounter = 0, store = DataMap.empty }

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
                                          WorkerThread _ chan <- getWorkerThread workerPoolMVar initialEnvironment
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
               e' <- work sock e
               putWorkerThread workerPoolMVar chan
               mainLoop e'{hitCounter = (hitCounter e') + 1}


work :: Socket -> Environment -> IO Environment
work sock' e  = do
    r <- getRequest sock'
    e' <- case r of
                Just req@(Request uri@(URI _ _ _ query _) method headers body) -> do putStrLn "request received"
                                                                                     id <- getParmValue "id" query
                                                                                     let x = case id of
                                                                                                Nothing -> Nothing
                                                                                                Just idValue -> (Map.lookup idValue $ store e) :: Maybe Int
                                                                                     let (response, e') = buildResponse (id, x) uri e
                                                                                     sendResponse sock' response
                                                                                     return e'
                                                                               
                Nothing -> do 
                    putStrLn "error caught..."
                    return e
    sClose sock'
    return e'

buildResponse ::(Maybe String, Maybe Int) -> URI -> Environment -> (String , Environment)
buildResponse (id, Just sesCounter) uri@(URI scheme _ path query fragment) e@(Environment counter map) = result 
    where result =  case id of
                          Nothing -> (base , e)
                          Just idvalue -> (base ++  "   sessionId [" ++ idvalue ++ "] sessionCounter[" ++ show sesCounter ++ "]" , Environment counter  (Map.insert  idvalue (sesCounter + 1) map))
          base =  "counter[ " ++ show counter ++ "] <br><br> " ++
                  "You have requested-- scheme[" ++ scheme ++ 
                  "] path [" ++ path ++ "] query [" ++ query ++ 
                  "] fragment ["++ fragment ++ "]" 
buildResponse (id, Nothing ) uri@(URI scheme _ path query fragment) e@(Environment counter map) = result 
    where result =  case id of
                          Nothing -> (base , e)
                          Just idvalue -> (base ++  "   sessionId [" ++ idvalue ++ "] sessionCounter[--0--]"  , Environment counter  (Map.insert  idvalue 1 map))
          base =  "counter[ " ++ show counter ++ "] <br><br> " ++
                  "You have requested-- scheme[" ++ scheme ++ 
                  "] path [" ++ path ++ "] query [" ++ query ++ 
                  "] fragment ["++ fragment ++ "]" 





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

------------------------------------------------
-- | Parsec Stuff
------------------------------------------------


p_query :: CharParser () [(String, Maybe String)]
p_query = char '?' >> p_pair `sepBy` char '&'

p_pair :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  value <- optionMaybe (char '=' >> many p_char)
  return (name, value)

p_char :: CharParser () Char
p_char = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> p_hex

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d


parseParms :: String -> IO ( Maybe ( Map String  (Maybe String)))
parseParms  input = do
    let result = parse p_query "(unknown)" input
    case result of
          Left e -> return Nothing
          Right m -> return $ Just $ fromList m

getParmValue parm input = do
    parseResult <- parseParms input
    case parseResult of
          Nothing -> return Nothing
          Just m ->  return $ join $  Map.lookup parm m
