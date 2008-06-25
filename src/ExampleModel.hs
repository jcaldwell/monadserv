
module ExampleModel where

import qualified Data.Map as DataMap

import MonadServ


data ExampleReturnObject =
  ExampleReturnObject
  {  retCode  :: String
  ,  retValue :: Int
  ,  labels   :: [String]
  }

instance JSONObject ExampleReturnObject where
      toJSON ( ExampleReturnObject retCode retValue labels) =                
               Object (DataMap.fromList [("retCode",  String retCode )
                                        ,("retValue", Number ((fromIntegral $ retValue)::Double))
                                        ,("labels", Array ( map (String) labels))
                                        ])
