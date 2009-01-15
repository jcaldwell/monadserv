{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadServ.HttpMonad where

import qualified Control.Exception as Ex
import Control.Monad.Reader
import Control.Monad.State
import System.IO
import MonadServ.JSONObject
import MonadServ.JSON

type BackendOutput =   String

-- | The type of results from server commands.  They are a modified
--   server state and possibly a server \"special\" action to execute.
--type CommandResult st = (st,Maybe (ServerSpecial st))
--type CommandResult st = (st, Maybe String)
type CommandResult st  = (st, Maybe Value )


-- | The type of commands which produce output for the server.
--   Changed to String from BackendOutput for now.
type OutputCommand = BackendOutput  -> IO ()

-- | The type of server commands.  This monad is a state monad layered over @IO@.
--   The type parameter @st@ allows the monad to carry around a package of
--   user-defined state.
newtype Srv st a = Srv { unSrv :: StateT (CommandResult st) (ReaderT OutputCommand IO) a }
   deriving (Monad, MonadIO)

-- | Special commands for the server framework.
data ServerSpecial st
  = ServerExit                  -- ^ Causes the shell to exit
  | ServerHelp (Maybe String)   -- ^ Causes the shell to print an informative message.
                               --   If a command name is specified, only information about
                               --   that command will be displayed
  | ServerNothing               -- ^ Instructs the shell to do nothing; redisplay the prompt and continue

---------------------------------------------
-- Monad Functions
---------------------------------------------

-- | Execute a server action
runSrv :: st -> OutputCommand -> Srv st () -> IO (CommandResult st)
runSrv st info = runSrvSpecial st Nothing info
--runSrv st info = (flip runReaderT) info . (flip execStateT) (st,Nothing) . unSrv


runSrvSpecial :: st -> Maybe Value -> OutputCommand -> Srv st () -> IO (CommandResult st)
runSrvSpecial st value info = (flip runReaderT) info . (flip execStateT) (st, value) . unSrv

-- | Output a tagged string to the console
srvPut :: BackendOutput -> Srv st ()
srvPut out = Srv (lift ask >>= \f -> liftIO (f out))

-- | Prints a regular output string
srvPutStr :: String -> Srv st ()
srvPutStr  = srvPut

-- | Prints regular output with a line terminator
srvPutStrLn :: String -> Srv st ()
srvPutStrLn = srvPutStr . (++"\n")

-- | Get the current server state
getSrvSt :: Srv st st
getSrvSt = Srv (get >>= return . fst)

-- | Set the server state
putSrvSt :: st -> Srv st ()
putSrvSt st = Srv (get >>= \ (_,spec) -> put (st,spec))

-- | Apply the given funtion to the server state
modifySrvSt :: (st -> st) -> Srv st ()
modifySrvSt f = getSrvSt >>= putSrvSt . f

--shellSpecial :: ShellSpecial st -> Sh st ()
--shellSpecial spec = Sh (get >>= \ (st,_) -> put (st,Just spec))
srvSpecial :: Value -> Srv st ()
srvSpecial spec = Srv (get >>= \ (st,_) -> put (st,Just spec))

getSrvSpecial :: Srv st (Maybe Value)
getSrvSpecial = Srv (get >>= return . snd)

putSrvSpecial :: Value -> Srv st ()
putSrvSpecial obj = Srv ( get >>= (\ (st,_) -> put (st,Just obj)))

instance MonadState st (Srv st) where
      get = getSrvSt
      put = putSrvSt


