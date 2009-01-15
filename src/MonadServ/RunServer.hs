
module MonadServ.RunServer where

import Text.ParserCombinators.Parsec
--import qualified Text.ParserCombinators.Parsec.Prim as Prim
import Data.Char
import Data.Time
import Data.Word
import Data.List (union)
import Control.Exception hiding (try)
import qualified Control.Exception as Ex
import Control.Concurrent.MVar     ( MVar, newEmptyMVar, tryTakeMVar, tryPutMVar, withMVar, takeMVar, putMVar, newMVar )
import Control.Concurrent.Chan     ( Chan, writeChan, readChan, newChan )
import Control.Concurrent          ( forkIO, myThreadId )
import Control.Monad               ( when, MonadPlus(..), join )
import Numeric (readHex)
import Network hiding (accept)
import Network.Socket
import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import System.IO
import System.Directory
import System.Locale
import System.Time
import qualified Data.Map as DataMap hiding (filter, union)
import Data.ByteString as ByteString (readFile, unpack)
import Text.PrettyPrint.HughesPJ hiding (char)

import qualified MonadServ.JSON as JSON
import MonadServ.HttpMonad
import MonadServ.Types

-- Entry Point
runServer :: ServerConfig st -> ServerBackend bst -> st -> IO st
runServer config backend i = Ex.bracket setup exit (\iss -> acceptLoop iss)
  where
      setup = do
          initEnv   <- initialEnvironment
          workerPoolMVar <- newMVar $ WorkerPool 0 [] []
          thVar     <- newEmptyMVar
          bst       <- initBackend backend
          sck       <- listenOn (PortNumber (fromIntegral $ mainPort config))
          when (historyEnabled config) (do
                                           putStrLn "Starting....")
          return InternalServerState
                     { envVar         = initEnv
                     , evalTest       = thVar
                     , cancelHandler  = handleINT
                     , backendState   = bst
                     , config         = config
                     , sock           = sck
                     , initState      = i
                     , pool = Just workerPoolMVar
                     }

      exit iss = do          
--        flushOutput backend (backendState iss)
          when (historyEnabled config) (do
                                           putStrLn "Stopping...")
          sClose (sock iss)
          shutdownBackend backend (backendState iss)

      acceptLoop  :: InternalServerState st bst -> IO st
      acceptLoop iss = do (sock', sockAddr) <- accept (sock iss)
                          WorkerThread _ chan <- getWorkerThread iss
                          writeChan chan sock'
                          acceptLoop iss

------------------------------------------------
-- | Worker stuff
------------------------------------------------
workerLoop :: InternalServerState st bst -> Chan Socket -> IO ()
workerLoop iss@(InternalServerState {pool = (Just workerPoolMVar)}) chan
    = do mainLoop iss
    where
        mainLoop iss
            = do sock <- readChan chan
                 iss' <- work sock iss
                 putWorkerThread workerPoolMVar chan
                 mainLoop iss'

    
work :: Socket -> InternalServerState st bst -> IO (InternalServerState st bst)
work sock' iss = do
    r <- getRequest sock'
    iss' <- case r of
                  Just req -> do
                       putStrLn "request received"
                       (result, iss') <- handleRequest req iss
                       sendResponse sock' result
                       return iss'
                  Nothing -> do
                       putStrLn "error caught..."
                       return iss
    sClose sock'
    return iss'

handleRequest :: Request -> InternalServerState st bst -> IO (String, InternalServerState st bst)
handleRequest  req@(Request uri@(URI scheme _ path query fragment) _ _ _) iss
   = do mSessionId <- getParmValue "id" query
        store <- takeMVar (storeMVar (envVar iss))
        (resultString, resultEnv) <- case mSessionId of
                                           Nothing -> return $ handleNoSession store 
                                           Just sessionId -> do let sessionEnv = (DataMap.lookup sessionId store) -- :: Maybe (MVar a)
                                                                handleSession sessionId sessionEnv store iss
        putMVar (storeMVar (envVar iss)) resultEnv
        return (resultString, iss)
    where handleNoSession store = ("NO SESSION", store)
          handleSession sessionId Nothing store iss = 
              do sesMVar <- newMVar $ (initState  ) iss
                 return ("RUN 1" , DataMap.insert sessionId sesMVar store)
          handleSession sessionId (Just sessionValueMV) store iss = 
              do sessionValue <- takeMVar sessionValueMV
                 let sessionValue' = id sessionValue
                     (rs,rE) = ("RUN 2 " ++ sessionId ++ "  :", DataMap.insert sessionId sessionValueMV store)
                 putMVar sessionValueMV sessionValue'
                 return (rs, rE)



{--

serverLoop :: ServerConfig st -> ServerBackend bst -> InternalServerState st bst -> st -> IO st
serverLoop config backend iss = loop
 where
   bst = backendState iss

--   loop :: st -> IO st
   loop st = do
---       flushOutput bst bst
       runSrv st (outputString backend bst Nothing) (beforePrompt config) 
       (handle,hostName,portNumber) <- accept $ sock iss

       inp <- getInput handle hostName

       case inp  of
             Nothing -> return st
             Just request@(Request op url hdrs msg) -> handleInput handle request st



   getInput :: Handle -> String -> IO (Maybe Request)
   getInput handle hostName = do
       x <- Control.Exception.catch (do
		s <- hGetContents handle
--                putStrLn $ "contents [" ++ s  ++ "]"
		let parseResult=Text.ParserCombinators.Parsec.parse parseRequest "request" s
		case parseResult of
		      Left err -> do
			hPutStrLn handle (show err)
			return Nothing
		      Right (operation, url, headers, msgcontent)  -> do
                        return $ Just (Request operation url headers msgcontent)
		)
                (\e -> do
		     write500 handle (show e)
		     return Nothing
		)
       return x


   handleInput handle request@(Request op url hdrs msg) st' = do
       case lookup (tail url) (serverCommands config) of
           Just f -> executeCommand handle request st' f
           Nothing -> serveContent handle request st'



   executeCommand handle r@(Request op url hdrs msg) st' f = do
       runSrv st' (outputString backend bst Nothing) (srvPutStrLn $ op ++ ": [" ++ url ++ "]")
       let parseResult= MonadServ.JSON.parse msg
       (st'' , x) <- runSrvSpecial st' parseResult (outputString backend bst (Just handle)) (f config)
--       (st'', x) <- runSrv st' (outputString backend bst (Just handle)) (f config)

       case x of
             Just res -> do
                 let  parseResult = renderStyle (style {mode=OneLineMode}) (toDoc res)
                 runSrv st' (outputString backend bst (Just handle)) (srvPutStrLn $ "testCallback(" ++ parseResult ++ ");")
             Nothing -> runSrv st' (outputString backend bst Nothing) (srvPutStrLn $ url ++ " returns no JSON Object.")
       hClose handle
       loop st''

   serveContent handle r@(Request op url hdrs msg) st' = do
       runSrv st' (outputString backend bst Nothing) (srvPutStrLn $ " [" ++ op ++"] " ++ url ++ " --" )
       let fileName=docRoot config ++ url
       exists <- doesFileExist (fileName)
       if exists
	 then do
               let (mimetype,binary)= case DataMap.lookup (getExtension url) mimeMapping of
					    Nothing -> ("text/plain",False)
                                            Just (s,b) -> (s,b)
	       if binary
		 then do
                       result <- ByteString.readFile (fileName)
		       -- (encode (unpackList result))
		       -- writeContent handle mimetype (octetsToString (unpackList result)) False
                       writeContent handle mimetype (octetsToString (unpack result)) False
		 else do
		       result <- System.IO.readFile (fileName)
		       writeContent handle mimetype result False
	 else do
	       write404 handle
       loop st'

--}

--what to do when we are interrupted.
handleINT :: IO ()
handleINT = return ()


------------------------------------------------
-- | Worker Pool stuff
------------------------------------------------

getWorkerThread  :: InternalServerState st bst -> IO WorkerThread
getWorkerThread iss@(InternalServerState {pool = (Just mv) } )  = 
  do wp <- takeMVar mv
     case wp of
       WorkerPool n [] bs -> 
         do chan <- newChan
            tid <- forkIO $ workerLoop iss chan
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


parseParms :: String -> IO ( Maybe ( DataMap.Map String  (Maybe String)))
parseParms  input = do
    let result = parse p_query "(unknown)" input
    case result of
          Left e -> return Nothing
          Right m -> return $ Just $ DataMap.fromList m

getParmValue parm input = do
    parseResult <- parseParms input
    case parseResult of
          Nothing -> return Nothing
          Just m ->  return $ join $  DataMap.lookup parm m
