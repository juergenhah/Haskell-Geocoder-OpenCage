{-|
Module      : HTTPWrapper
Description : Requests a URL and returns the response body
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

This module handles HTTP requests using the simpleHTTP function. It deals with the most
exceptions.
-}
module HTTPWrapper
  ( download
  ) where

import Control.Exception as E (SomeException, try)
import Data.ByteString.Char8 as Bs (ByteString, pack)
import Network.HTTP as HTTP (getRequest, simpleHTTP)
import Network.HTTP.Base as Base
import Network.HTTP.Headers as Headers
import Network.Stream as Stream

-- | requests the url with the parameter List via GET, 
-- returns an error message or the body as a ByteString
download ::
     String -- ^ URL to request
  -> [(String, String)] -- ^ parameter list attached to the request URL e.g: [("p1","test"),("p2","today")]
  -> IO (Either String ByteString) -- ^ Left includes the error with a message, Right the response body
download url requestParameter = do
  putStrLn requestURL
  downloadURL requestURL
  where
    requestURL = url ++ Base.urlEncodeVars requestParameter

-- | function requests the URL and returns with a Error and a Message or with the body as ByteString
-- can deal with redirects, see RealWorldHaskell for a description
downloadURL :: String -> IO (Either String ByteString) --
downloadURL url = do
  let headers = [mkHeader Headers.HdrUserAgent "haskell-opencage-geocoder"]
      request = Headers.insertHeaders headers (HTTP.getRequest url)
  result <-
    E.try (HTTP.simpleHTTP request) :: IO (Either E.SomeException (Stream.Result (Response String)))
  case result of
    Left x -> return $ Left ("Problem with simpleHTTP :" ++ show x)
    Right resp ->
      case resp of
        Left x ->
          case x of
            Stream.ErrorReset -> return $ Left "ErrorReset: "
            Stream.ErrorClosed -> return $ Left "ErrorClosed: "
            Stream.ErrorParse y -> return $ Left ("ErrorClosed: " ++ show y)
            Stream.ErrorMisc y -> return $ Left ("ErrorClosed: " ++ show y)
        Right r ->
          case rspCode r of
            (2, _, _) -> return $ Right (Bs.pack $ rspBody r)
            (3, _, _) ->
              case findHeader Headers.HdrLocation r --redirect
                    of
                Nothing -> return $ Left (show r)
                Just address -> downloadURL address
            _ -> return $ Left ("unknown Problem occurred")
