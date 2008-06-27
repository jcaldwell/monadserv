
module RunServer where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Prim as Prim
import Data.Char
import Control.Exception hiding (try)
import qualified Control.Exception as Ex
import Control.Concurrent.MVar     ( MVar, newEmptyMVar, tryTakeMVar, tryPutMVar, withMVar, takeMVar, putMVar )
import Control.Monad               ( when, MonadPlus(..) )
import Network
import System.IO
import qualified Data.Map as DataMap

import HttpMonad
import Types

-- Entry Point
runServer :: ServerConfig st -> ServerBackend bst -> st -> IO st
runServer config backend init = Ex.bracket setup exit (\iss -> executeServer config backend iss init )
  where
      setup = do 
          evVar     <- newEmptyMVar
          thVar     <- newEmptyMVar
          bst       <- initBackend backend
          sck       <- listenOn (PortNumber (fromIntegral $ port config))

          return InternalServerState
                     { evalVar        = evVar
                     , evalTest       = thVar
                     , cancelHandler  = handleINT 
                     , backendState   = bst
                     , sock           = sck
                     }
      
      exit iss = do
            sClose (sock iss)
            shutdownBackend backend (backendState iss)
      
      executeServer :: ServerConfig st 
                    -> ServerBackend bst
                    -> InternalServerState st bst 
                    -> st 
                    -> IO st
      executeServer config backend iss init = do
          when (historyEnabled config) (do
             putStrLn "Starting....")
          
          final <- serverLoop config backend iss init
          
          when (historyEnabled config) (do
             putStrLn "Stopping...")
          
--        flushOutput backend (backendState iss)
          return final


serverLoop :: ServerConfig st -> ServerBackend bst -> InternalServerState st bst -> st -> IO st
serverLoop config backend iss = loop
 where
   bst = backendState iss

--   loop :: st -> IO st
   loop st = do 
---       flushOutput bst bst
       runSh st (outputString backend bst Nothing) (beforePrompt config)  --might remove beforePrompt to beforeAccept
       (handle,hostName,portNumber) <- accept $ sock iss

       inp <- getInput handle hostName

       case inp  of
             Nothing -> return st
--             Just a@(Request _ url _ _) -> evaluateInput handle (tail url) st 
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
           Just f -> executeCommand handle f request st'
           Nothing -> evaluateInput handle url st'

{--       
       case lookup inp (

       case runRegex (commandsRegex desc) inp' of
             (x,inp''):_ -> executeCommand handle request x inp'' st'
             []          -> evaluateInput handle inp st'
--}

   executeCommand handle f  a@(Request op url hdrs msg) st' = do
--       runSh st' (outputString backend bst) (srvPutStrLn $ "command from URL [" ++ (tail url) ++ "]" )
--       runSh st' (outputString backend bst) (srvPutStrLn $ "operation [" ++ op ++ "]" )
--       runSh st' (outputString backend bst) (srvPutStrLn $ "url       [" ++ url ++ "]" )
--       runSh st' (outputString backend bst) (srvPutStrLn $ "hdrs      [" ++ (show hdrs) ++ "]" )
--       runSh st' (outputString backend bst) (srvPutStrLn $ "msg       [" ++ msg ++ "]" )

       (st'', _) <- runSh st' (outputString backend bst (Just handle)) (f config)

       hClose handle
       loop st''

   evaluateInput handle inp st' = do
       runSh st' (outputString backend bst (Just handle)) (srvPutStrLn "evaluating input...")
       hClose handle
       loop st'

--what to do when we are interrupted.
handleINT :: IO ()
handleINT = return ()

----------------------------------------
parseRequest :: Parser (Operation,URL,Headers,MsgContent)
parseRequest = do
	op <- many1 letter <?> "operation"
	skipMany (char ' ')
	url <- manyTill anyChar (Prim.try (char ' '))
	manyTill anyChar (Prim.try pCRLF)
	headM <- manyTill parseHeaders (Prim.try pCRLF)
	let headers=DataMap.fromList headM
	let cl=DataMap.lookup "content-length" headers 
	case cl of
		Nothing -> do
			--content <- manyTill anyChar (Prim.try pCRLF)
			return (op,url,headers,"")
		Just c -> do
			content <- count (atoi c) anyChar
			return (op,url,headers,content)
		

atoi :: String -> Int
atoi s=foldl (\ y x -> Data.Char.digitToInt(x) + (y*10)) 0 s
	
parseHeaders :: Parser (String,String)
parseHeaders = do
	name <- many (noneOf ":\n\r") <?> "header name"
	let nameL=map toLower name
	char ':' <?> ": after header name"
	skipMany (char ' ')
	value <- many (noneOf "\n\r") <?> "header value"
	pCRLF <?> "line after header"
	return (nameL,value)

{-
testParseHeaders = do
	let parseResult=parse parseRequest "request" "GET / HTTP/1.1\r\nName: Value\r\nName2: Value2\r\n\r\ncontent"
	case parseResult of
		Left err -> show err
		Right (op,url,headers,content) -> show ("op:"++op++"\nurl:"++url++ "\nheaders:"++(show (DataMap.toList headers))++ "\ncontent:"++content)
	-}

-- | RFC 2616 CRLF
pCRLF :: Parser String
pCRLF = try (string "\r\n" <|> string "\n\r") <|> string "\n" <|> string "\r"

crossDomain :: String
crossDomain = "<?xml version=\"1.0\"?><cross-domain-policy><allow-access-from domain=\"*\" to-ports=\"9000\" secure=\"false\"/></cross-domain-policy>"

writeContent :: Handle -> String -> String -> Bool -> IO()	
writeContent handle ctype content cache= do
	hPutStr handle "HTTP/1.1 200 OK\r\n"
	hPutStr handle "Server: Haskell Server\r\n"
	hPutStr handle ("Content-Type: "++ctype++"\r\n")
	hPutStr handle ("Content-Length: "++(show (length content))++"\r\n")
	if not cache 
		then do
			hPutStr handle ("Cache-Control: no-cache\r\n") --HTTP 1.1
			hPutStr handle ("Pragma: no-cache\r\n") --HTTP 1.0
			hPutStr handle ("Expires: 0\r\n") --prevents caching at the proxy server
		else do
			return ()
	hPutStr handle "\r\n"
	hPutStr handle content
	hPutStr handle "\r\n"
	hPutStr handle "\r\n"

write404 :: Handle -> IO()
write404 handle = do
	hPutStr handle "HTTP/1.1 404 Not Found\r\n"
	hPutStr handle "\r\n"
	hPutStr handle "\r\n"
	hPutStr handle "\r\n"
		
write500 :: Handle -> String -> IO()
write500 handle message= do
	hPutStr handle "HTTP/1.1 500 Internal Server Error\r\n"
	hPutStr handle ("Content-Type: text/plain\r\n")
	hPutStr handle "\r\n"
	hPutStr handle message
	hPutStr handle "\r\n"
	hPutStr handle "\r\n"
	
type Operation = String
type URL = String
type Headers= DataMap.Map String String
type MsgContent = String

data Request = Request { operation  :: Operation
                       , url        :: URL
                       , headers    :: Headers
                       , msgContent :: MsgContent
                       }


