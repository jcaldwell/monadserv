
module MonadServ.RunServer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Prim as Prim
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
import Data.Unique.Id

-- Entry Point
runServer :: ServerConfig st -> ServerBackend bst -> st -> IO st
runServer config backend i = Ex.bracket setup exit (\iss -> acceptLoop iss)
  where
      setup = do
          initEnv   <- initialEnvironment
          workerPoolMVar <- newMVar $ WorkerPool 0 [] []
          initialSupply <- initIdSupply 'm'
          idSupplyMVar <- newMVar $ initialSupply
          thVar     <- newEmptyMVar
          bst       <- initBackend backend
          sck       <- listenOn (PortNumber (fromIntegral $ mainPort config))
          when (historyEnabled config) (do
                                           putStrLn "Starting....")
          return InternalServerState
                     { envVar         = initEnv
                     , evalTest       = thVar
                     , cancelHandler  = handleINT
                     , idSupplyMVar   = idSupplyMVar
                     , backendState   = bst
                     , backendService = backend
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
    = do mainLoop
    where
        mainLoop 
            = do sock <- readChan chan
                 work sock iss
                 putWorkerThread workerPoolMVar chan
                 mainLoop 

    
work :: Socket -> InternalServerState st bst -> IO ()
work sock' iss = do
    r <- getRequest sock'
    case r of
          Just req -> do
                     putStrLn "request received"
                     result <- handleRequest req iss
                     sendResponse sock' result
          Nothing -> do
                     putStrLn "error caught..."
    sClose sock'


handleRequest :: Request -> InternalServerState st bst -> IO String
handleRequest  req@(Request uri@(URI scheme _ path query fragment) _ _ rqBody) iss
   = do mSessionId <- getParmValue "id" query
        mfunction <- getParmValue "function" query
        store <- takeMVar (storeMVar (envVar iss))
        (resultString, resultEnv) <- case mSessionId of
                                           Nothing ->  handleNoSession store iss
                                           Just sessionId -> do let sessionEnv = (DataMap.lookup sessionId store) -- :: Maybe (MVar a)
                                                                handleSession sessionId sessionEnv store iss mfunction
        putMVar (storeMVar (envVar iss)) resultEnv
        return resultString
    where handleNoSession store iss = 
              do idSupply <- takeMVar (idSupplyMVar iss)
                 let (sessionId,newSupply) =  getNextId idSupply
                 putMVar (idSupplyMVar iss) newSupply
                 return (show sessionId, store)
          handleSession sessionId Nothing store iss mfunction  = 
              do let newState = initState iss
                 (result, s) <- runRequest rqBody (tail path) newState iss mfunction False
                 sesMVar <- newMVar s
                 return (result , DataMap.insert sessionId sesMVar store)
          handleSession sessionId (Just sessionValueMV) store iss mfunction  = 
              do sessionValue <- takeMVar sessionValueMV
                 (result,s) <- runRequest rqBody (tail path) sessionValue iss mfunction True
                 putMVar sessionValueMV s
                 let (rs,rE) = (result , DataMap.insert sessionId sessionValueMV store)
                 return (rs, rE)

runRequest :: String -> String -> st -> InternalServerState st bst -> Maybe String -> Bool -> IO (String,st)
runRequest rqBody u st iss mfunction flag = do
    putStrLn ("flag: " ++ show flag)
    runRequest' rqBody u st iss mfunction

runRequest' :: String -> String -> st -> InternalServerState st bst -> Maybe String -> IO (String,st)
runRequest' rqBody u st iss@(InternalServerState {backendState = bst, config = config', backendService = bservice}) mfunction = do
    runSrv st (outputString bservice bst Nothing) (beforePrompt config')
    case lookup u (serverCommands config') of
          Just f -> executeCommand u st f
          Nothing -> serveContent u  st
    where executeCommand url' st' f = do
              runSrv st' (outputString bservice bst Nothing) (srvPutStrLn $ "--- url[" ++ url' ++ "]")
              let parseResult= JSON.parse rqBody
              (st'', x) <- runSrvSpecial st' parseResult (outputString bservice bst Nothing) (f config')
              case x of
                    Just res -> do
                         let  parseResult = renderStyle (style {mode=OneLineMode}) (JSON.toDoc res)
                              parseResult' = case mfunction of
                                                   Just function -> function ++"("++parseResult ++");"
                                                   Nothing -> parseResult
                         runSrv st' (outputString bservice bst Nothing) (srvPutStrLn parseResult')
                         return (parseResult', st'')
                    Nothing -> do
                         let result = " returns no JSON Object."
                         runSrv st' (outputString bservice bst Nothing) (srvPutStrLn $ "::" ++ result)
                         return (result, st'')
          serveContent url'  st' = do
              runSrv st' (outputString bservice  bst Nothing) (srvPutStrLn $ " [" ++ "OP" ++"] " ++ url' ++ " --" )
              let fileName= docRoot config' ++ url'
              exists <- doesFileExist (fileName)
              if exists
	        then do
                      let (mimetype,binary)= case DataMap.lookup (getExtension url') mimeMapping of
					           Nothing -> ("text/plain",False)
                                                   Just (s,b) -> (s,b)
	              if binary
		        then do
                              result <- ByteString.readFile (fileName)
		              -- (encode (unpackList result))
		              -- writeContent handle mimetype (octetsToString (unpackList result)) False
                              --writeContent handle mimetype (octetsToString (unpack result)) False
                              return  ( octetsToString (unpack result), st')
		        else do
		              result <- System.IO.readFile (fileName)
                              return (result,st')
	        else do
	              return ("404 Not FOUND",st')



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


octetsToString :: [Word8] -> String
octetsToString = map (toEnum . fromIntegral)
 
mimeMapping :: DataMap.Map String (String,Bool)
mimeMapping=DataMap.fromList[("swf",("application/x-shockwave-flash",True)),
  ("html",("text/html",False)),("xml",("text/xml",False))]
 
getExtension:: FilePath -> String
getExtension s=map toLower (reverse (takeWhile (/= '.') (reverse s)))
 
getNextId :: IdSupply -> (Id, IdSupply)
getNextId supply = 
      let newSupply = snd $ splitIdSupply supply
      in (idFromSupply supply, newSupply)