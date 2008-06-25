
module JSONObject where

import JSON

class JSONObject a where
      toJSON :: a -> Value