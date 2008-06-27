
module Test where

import HttpMonad
import RunServer
import Types

data ExampleAppState = 
  ExampleAppState 
  { showCount   :: Bool      -- ^ If true, show the number of reductions at each step
  , histFile    :: Maybe String -- ^ A file for command history
  , loadedValue :: Int
  }


-- | Default settings for all elements of shell state.
initialExampleAppState :: ExampleAppState
initialExampleAppState  =
  ExampleAppState
  {  showCount   = False
   , histFile    = Nothing
   , loadedValue = 4
  }

test = myShell initialExampleAppState

myShell :: ExampleAppState -> IO ExampleAppState
myShell init = do
    let
      desc =
         (mkServerConfig commands )
         { port  = 8080
         , docRoot = "/var/www/" 
         }
    runServer desc (templateBackend "JSON") init

commands :: [ServerCommand ExampleAppState]
commands =
  [ 
    ( "ex1" , (\_ -> someActionOp  ))
  , ( "ex2" , (\_ -> exampleActionOp  ))
  , ( "ex3" , (\_ -> anotherExampleOp  ))
  , ( "ex4" , (\_ -> wtfActionOp ))
  ]


wtfActionOp :: Srv ExampleAppState ()
wtfActionOp = do
    x <- getSrvSpecial
    putSrvSpecial "\"wtf\""
    y <- getSrvSpecial
    a <- getSrvSpecial
    srvPutStrLn $ (show x ) ++ " -- " ++ (show y) ++ " -- " ++ (show a)
    b <- getSrvSpecial
    srvPutStrLn $ " -- " ++ (show b) ++ " -- "
--    modifySrvSt $ (\st -> st )

someActionOp :: Srv ExampleAppState ()
someActionOp = do
    x <- getSrvSpecial
    case x of
          Nothing -> do 
                   srvPutStrLn "nothing found."
                   putSrvSpecial "magics"
          Just txt -> srvPutStrLn $ "looks like we found some " ++ txt ++ ".  "

--someActionOp = srvPutStrLn "inside some action op. "

exampleActionOp :: Srv ExampleAppState ()
exampleActionOp = do
    myState <- getSrvSt
    let x = show $ loadedValue myState
    srvPutStrLn x

anotherExampleOp :: Srv ExampleAppState ()
anotherExampleOp = modifySrvSt $ 
                   (\st -> st { loadedValue = 20  } )

