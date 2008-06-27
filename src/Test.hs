
module Test where

import qualified Data.Map as DataMap
import HttpMonad
import RunServer
import Types
import JSONObject
import JSON

import ExampleModel


data ExampleAppState = 
  ExampleAppState 
  { showCount   :: Bool
  , histFile    :: Maybe String
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
  , ( "ex5" , (\_ -> yaeOp ))
  , ( "ex6" , (\_ -> retActionOp ))
  ]

--ppJSON x =  renderStyle (style{mode=OneLineMode}) (toDoc x)

wtfActionOp :: Srv ExampleAppState ()
wtfActionOp = putSrvSpecial $ String "wtf"

retActionOp :: Srv ExampleAppState ()
retActionOp = do
    let x  = ExampleReturnObject { retCode = "pass", retValue = 15, labels = ["test", "some", "values", "here"] }
    let y = toJSON x
    putSrvSpecial y

someActionOp :: Srv ExampleAppState ()
someActionOp = do
    x <- getSrvSpecial
    case x of
          Nothing -> do 
                   srvPutStrLn "nothing found."
                   putSrvSpecial $ String "magics"
          Just txt -> srvPutStrLn $ "looks like we found some " ++ (show txt) ++ ".  "

exampleActionOp :: Srv ExampleAppState ()
exampleActionOp = do
    myState <- getSrvSt
    let x = show $ loadedValue myState
    srvPutStrLn x

anotherExampleOp :: Srv ExampleAppState ()
anotherExampleOp = modifySrvSt $ 
                   (\st -> st { loadedValue = 20  } )

yaeOp :: Srv ExampleAppState ()
yaeOp = do 
    myState <- getSrvSt
    let x = loadedValue myState + 1
    modifySrvSt $ (\st -> st { loadedValue = x  })
    srvPutStrLn $ show x
