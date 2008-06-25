
module MonadServ.JSONObject where

import MonadServ.JSON

class JSONObject a where
      toJSON :: a -> Value