{-# LANGUAGE OverloadedStrings #-}

module Todo.Util.Logger (
  isLoggableRoute,
  logger,
) where

import Network.HTTP.Types (Status)
import Network.Wai (Request, rawPathInfo)
import Network.Wai.Logger (ApacheLogger)

{- |
We wrap the original ApacheLogger as we don't want our application
to blatantly log all received requests.
-}
logger :: ApacheLogger -> Request -> Status -> Maybe Integer -> IO ()
logger apLogger request status mCode =
  let route = decodeUtf8 $ rawPathInfo request
   in when (isLoggableRoute route) $
        apLogger request status mCode

{- |
This function tells the caller if the provided relative route
is to be filtered out for whatever purpose.
-}
isLoggableRoute :: Text -> Bool
isLoggableRoute path =
  let filterableRoutes = ["/healthz"]
   in path `notElem` filterableRoutes
