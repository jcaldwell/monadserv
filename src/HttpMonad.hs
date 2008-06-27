{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HttpMonad where 

import qualified Control.Exception as Ex

import Control.Monad.Reader
import Control.Monad.State
import System.IO

type BackendOutput =   String

-- | The type of results from shell commands.  They are a modified
--   shell state and possibly a shell \"special\" action to execute.
type CommandResult st = (st,Maybe (ServerSpecial st))


-- | The type of commands which produce output on the shell console.
--   Changed to String from BackendOutput for now.
type OutputCommand = BackendOutput  -> IO ()

-- | The type of shell commands.  This monad is a state monad layered over @IO@.
--   The type parameter @st@ allows the monad to carry around a package of
--   user-defined state.
newtype Srv st a = Srv { unSrv :: StateT (CommandResult st) (ReaderT OutputCommand IO) a }
   deriving (Monad, MonadIO)

-- | Special commands for the shell framework.
data ServerSpecial st
  = ServerExit                  -- ^ Causes the shell to exit 
  | ServerHelp (Maybe String)   -- ^ Causes the shell to print an informative message.
                               --   If a command name is specified, only information about
                               --   that command will be displayed
  | ServerNothing               -- ^ Instructs the shell to do nothing; redisplay the prompt and continue

---------------------------------------------
-- Monad Functions
---------------------------------------------

-- | Execute a shell action
runSh :: st -> OutputCommand -> Srv st () -> IO (CommandResult st)
runSh st info = (flip runReaderT) info . (flip execStateT) (st,Nothing) . unSrv

-- | Output a tagged string to the console
srvPut :: BackendOutput -> Srv st ()
srvPut out = Srv (lift ask >>= \f -> liftIO (f out))

-- | Prints a regular output string
srvPutStr :: String -> Srv st ()
srvPutStr  = srvPut 

-- | Prints regular output with a line terminator
srvPutStrLn :: String -> Srv st ()
srvPutStrLn = srvPutStr . (++"\n")

-- | Get the current shell state
getSrvSt :: Srv st st
getSrvSt = Srv (get >>= return . fst)

-- | Set the shell state
putSrvSt :: st -> Srv st ()
putSrvSt st = Srv (get >>= \ (_,spec) -> put (st,spec))

-- | Apply the given funtion to the shell state
modifySrvSt :: (st -> st) -> Srv st ()
modifySrvSt f = getSrvSt >>= putSrvSt . f
