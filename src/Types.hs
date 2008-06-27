
module Types where

import Data.Maybe
import Control.Concurrent.MVar     ( MVar, newEmptyMVar, tryTakeMVar, tryPutMVar, withMVar, takeMVar, putMVar )
import Network
import System.IO   ( stdout, stderr, stdin, hFlush, hPutStr, hPutStrLn
	           , hGetLine, hGetChar, hGetBuffering, hSetBuffering, Handle
                   , BufferMode(..)
                   )

import HttpMonad

type ServerCommand st = ( String , ServerConfig st -> Srv st () )
--type ServerCommand = (String, String )

data ServerConfig st
   = SrvDesc
   { serverCommands     :: [ServerCommand st]      
--   , evaluateFunc       :: String -> Srv st ()    
   , greetingText       :: Maybe String             
   , beforePrompt       :: Srv st ()                 
   , prompt             :: st -> IO String          
--   , exceptionHandler   :: Ex.Exception ->  Sh st ()                 
--   , historyFile        :: Maybe FilePath         
   , maxHistoryEntries  :: Int                      
   , historyEnabled     :: Bool                     
   , port               :: Int
   , docRoot            :: FilePath                                                    
   } 

data InternalServerState st bst
   = InternalServerState
     { evalVar         :: MVar (Maybe (st))
     , evalTest        :: MVar String
     , cancelHandler   :: IO ()
     , backendState    :: bst
     , sock            :: Socket
     }

data ServerBackend bst
   = SrvBackend
     { initBackend                    :: IO bst
     , shutdownBackend                :: bst -> IO ()
     , outputString                   :: bst -> Maybe Handle -> BackendOutput -> IO ()
     , flushOutput                    :: bst -> IO ()
     , getSingleChar                  :: bst -> String -> IO (Maybe Char)
     , getInput                       :: bst -> String -> IO (Maybe String)
     , addHistory                     :: bst -> String -> IO ()
--     , setWordBreakChars              :: bst -> String -> IO ()
--     , getWordBreakChars              :: bst -> IO String
     , onCancel                       :: bst -> IO ()
--     , setAttemptedCompletionFunction :: bst -> CompletionFunction -> IO ()
     , setDefaultCompletionFunction   :: bst -> Maybe (String -> IO [String]) -> IO ()
     , completeFilename               :: bst -> String -> IO [String]
     , completeUsername               :: bst -> String -> IO [String]
     , clearHistoryState              :: bst -> IO ()
     , setMaxHistoryEntries           :: bst -> Int -> IO ()
     , getMaxHistoryEntries           :: bst -> IO Int
--     , readHistory                    :: bst -> FilePath -> IO ()
--     , writeHistory                   :: bst -> FilePath -> IO ()
     }

templateBackend :: a -> ServerBackend a
templateBackend bst = SrvBackend
     { initBackend                    = return bst
     , shutdownBackend                = \_ -> do putStrLn "ya'll come back now..."
                                                 return ()
     , outputString                   = \_ h -> basicOutput h
     , flushOutput                    = \_ -> do putStrLn "flush"
                                                 return ()
     , getSingleChar                  = \_ _ -> return Nothing
     , getInput                       = \_ _ -> return Nothing
     , addHistory                     = \_ _ -> return ()
--     , setWordBreakChars              = \_ _ -> return ()
--     , getWordBreakChars              = \_ -> return defaultWordBreakChars
     , onCancel                       = \_ -> return ()
--     , setAttemptedCompletionFunction = \_ _ -> return ()
     , setDefaultCompletionFunction   = \_ _ -> return ()
     , completeFilename               = \_ _ -> return []
     , completeUsername               = \_ _ -> return []
     , clearHistoryState              = \_ -> return ()
     , setMaxHistoryEntries           = \_ _ -> return ()
     , getMaxHistoryEntries           = \_ -> return 0
--     , readHistory                    = \_ _ -> return ()
--     , writeHistory                   = \_ _ -> return ()
     }


templateServerConfig =  SrvDesc
   { serverCommands     = []
   , greetingText       = Just "Welcome now my friends..."
   , prompt             =  \_ -> return "> " 
   , beforePrompt       = srvPutStrLn  "show before handling request..."
   , maxHistoryEntries  = 0
   , historyEnabled     = True
   , port               = 8080
   , docRoot            = "/var/www/"                                                    
   }



-- | Creates a simple shell description from a list of shell commmands and
--   an evalation function.
mkServerConfig :: [ServerCommand st]
                     -> ServerConfig st

mkServerConfig cmds =
   templateServerConfig
      { serverCommands = cmds
      }


basicOutput :: Maybe Handle -> BackendOutput -> IO ()
basicOutput (Just handle) out = do
    hPutStr stdout out
    hPutStr handle out
basicOutput Nothing out = hPutStr stdout out


