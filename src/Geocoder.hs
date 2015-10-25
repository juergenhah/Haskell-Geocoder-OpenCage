{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|
Module      : Geocoder
Description : Module to convert an address into coordinates.
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

Requests the <http://geocoder.opencagedata.com> geocoding service to get  latitude and longitude values from an address. Uses Aeson to convert the response from JSON to Haskell types.
-}
module Geocoder  (QueryParameters(..)
  ,defaultParams
  ,TokenToGeocode
  ,GeoCageDeveloperKey
  ,getAPIResponse
  ,getAPIResponseWith
  ,geocode
  ,geocodeWith
  ,geocodeGeoTokensList
  ,createGeocoderParams
  ,ResponseBody(..)
  ,Result(..)
  ,License(..)
  ,Rate(..)
  ,Status(..)
  ,Timestamp(..)
  ,Components(..)
  ,Location(..)
  ,Bounds (..)) where

import qualified Data.Text as T (Text,pack,unpack)
import GHC.Generics (Generic)
import Data.Aeson  hiding (Result) 

import Data.ByteString.Char8 as Bs (ByteString) 

import HTTPWrapper



-- * Request Type

-- | data type including all possible parameters OpenCage API offers
data QueryParameters = QueryParameters {
  para_q::T.Text, -- ^ Text to query
  para_key::T.Text, -- ^ OpenCage key
  para_language:: Maybe T.Text, -- ^ language for the result: IETF format language code 
  para_pretty::Maybe T.Text, -- ^ should the result be pretty printed ?
  para_bounds::Maybe T.Text, -- ^ rectangle hint: southwest corner, northeast corner
  para_country::Maybe T.Text, -- ^ country hint:  ISO 3166-1 Alpha 3
  para_fields::Maybe T.Text -- ^ comma separated list
}deriving (Show)


-- | minimal example of QueryParameters
-- can be used to create your own parameters
defaultParams :: QueryParameters
defaultParams = QueryParameters {
  para_q="",
  para_key="",
  para_language=Just "en",
  para_pretty=Nothing,
  para_bounds=Nothing,
  para_country=Nothing,
  para_fields=Nothing}

type TokenToGeocode = T.Text
type GeoCageDeveloperKey = T.Text

-- * Request functions

-- | Minimalistic version to geocode a token returning the whole Response
getAPIResponse :: TokenToGeocode -- ^ token to geocode
                -> GeoCageDeveloperKey -- ^ OpenCage key
                -> IO (Maybe ResponseBody) -- ^ if the geocoding was successful there is a ResultBody full of data returned
getAPIResponse tokenToGeocode developerKey = 
  getAPIResponseWith parameters
  where parameters = defaultParams {para_q= tokenToGeocode,para_key=developerKey}


-- | main geocode function, called by all other functions, deals with errors 
getAPIResponseWith :: QueryParameters -- ^ parameters included into the request
                    -> IO (Maybe ResponseBody) -- ^ if the geocoding was successful there is a ResultBody full of data returned
getAPIResponseWith parameters =do
  reqSuccesfull <-requestGeocage parameters
  case reqSuccesfull of
    Left err -> do   putStrLn "exception at OpenCage request, issue:"
                     print err
                     return Nothing
    Right response -> case eitherDecodeStrict response of
                             Left exception -> do putStrLn
                                                    "exception at ByteString to JSON conversion, issue:"
                                                  print exception
                                                  return Nothing
                             Right resultBody -> return $ Just resultBody
 where requestGeocage :: QueryParameters -> IO (Either String ByteString)
       requestGeocage params = download geocageUrl para
        where geocageUrl = "http://api.opencagedata.com/geocode/v1/json?" 
              para = convertQParamstoOptionsList params
 
-- | converts QueryParameters to a list of tupels needed by the HTTPWrapper module 
convertQParamstoOptionsList :: QueryParameters -- ^ data type including all request parameters 
                            -> [(String,String)] -- ^ list of tupels containing all request parameters
convertQParamstoOptionsList (QueryParameters q key lang pre boun cou fields)= [] ++
 [("q",T.unpack q),("key",T.unpack key)]
 ++ toParameter "language" lang 
 ++ toParameter "pretty" pre 
 ++ toParameter "bounds" boun
 ++ toParameter "country" cou
 ++ toParameter "fields" fields
 where toParameter :: String -> Maybe T.Text -> [(String,String)]
       toParameter param mValue = case mValue of
                                   Nothing -> []
                                   Just val -> [(param,T.unpack val)]


-- | geocodes the token and returns a list of results
geocode :: TokenToGeocode -- ^ token to geocode, empty if an error occured
         -> GeoCageDeveloperKey -- ^ OpenCage key
         -> IO [Result] -- ^ list of results
geocode tokentoGeocode developerKey = geocodeWith params
  where params = defaultParams {para_q=tokentoGeocode,para_key=developerKey}
  

-- | geocodes the token included into the QueryParameters respecting all values set in the QueryParameters 
-- returns an empty list if an exception occured or the result was empty
geocodeWith :: QueryParameters -- ^ parameters included into the request
             ->  IO [Result] -- ^ list of results, empty if an error occured
geocodeWith params = do 
  resp <- getAPIResponseWith params 
  case resp of
   Nothing -> return []
   Just r -> return $ results r

-- | geocodes a list of tokens and prints the results
geocodeGeoTokensList :: [TokenToGeocode] -- ^ list of tokens to geocode
                      -> GeoCageDeveloperKey -- ^ OpenCage key
                      -> IO() -- ^ prints the result
geocodeGeoTokensList geoTokens developerKey = do
 let geocodeparams = defaultParams {para_key=developerKey,
                                               para_language= Just$ T.pack  "en",
                                               para_country = Just $ T.pack "DEU"}
 let params =  createGeocoderParams geocodeparams geoTokens
 res <-  mapM geocodeWith params
 print res

-- | packs tokens into QueryParameters and returns this as a list
-- if you want to specify your own parameters use this function
createGeocoderParams :: QueryParameters -- ^ parameters that should be used for all tokens in the list
                      -> [TokenToGeocode] -- ^ list of tokens to geocode
                      -> [QueryParameters] -- ^ list of parameters, for every token one QueryParameter entry is included
createGeocoderParams params geoTokens = zipWith (\p t -> p {para_q=t}) 
                   (replicate (length geoTokens) params) 
                   geoTokens

-- * data types for the geocoding result
 

-- | data type respecting OpenCage response encoded as JSON 
data ResponseBody = ResponseBody { 
  licenses :: [License], 
  rate :: Rate,
  results :: [Result],
  status :: Status,
  thanks :: T.Text,
  timestamp :: Timestamp,
  total_results :: Integer
} deriving (Show,Generic)
instance FromJSON ResponseBody
  
data License = License { 
  name :: T.Text, 
  url :: T.Text
} deriving (Show, Generic)
instance FromJSON License 

data Rate = Rate {
  limit :: Integer, 
  remaining::Integer, 
  reset::Double 
} deriving (Show, Generic)
instance FromJSON Rate

data Result = Result { 
  bounds :: Bounds, 
  components:: Components, 
  confidence:: Integer, 
  formatted::  T.Text , 
  geometry:: Location
} deriving (Show,Generic)

instance FromJSON Result where
  parseJSON (Object v) = Result    <$>
                         v .: "bounds" <*>
                         v .: "components"<*>
                         v .: "confidence"<*>
                         (v .:? "formatted" .!= "") <*> 
                         v .: "geometry"
  parseJSON _ = undefined

data Status = Status { 
  code::Integer, 
  message::T.Text
} deriving (Show, Generic)
instance FromJSON Status

data Timestamp = Timestamp { 
  created_http::T.Text, 
  created_unix:: Integer
} deriving (Show,Generic)
instance FromJSON Timestamp

data Bounds = Bounds { 
  northeast:: Location, 
  southwest::Location
} deriving (Show, Generic)
instance FromJSON Bounds

data Location = Location { 
  lat::Double, 
  lng :: Double
} deriving (Show, Generic)
instance FromJSON Location

data Components = Components { 
  country ::  T.Text, 
  country_code::  T.Text,
  county::  T.Text,
  state:: T.Text,
  city::  T.Text,
  road ::  T.Text
} deriving (Show,Generic)

-- | not all components are included into the response, if so there is an empty string as default value
instance FromJSON Components where 
  parseJSON (Object v) = Components    <$>
                         (v .:? "country" .!= "") <*>
                         (v .:? "country_code" .!= "") <*>
                         (v .:? "county" .!= "") <*>
                         (v .:? "state" .!= "") <*>
                         (v .:? "city" .!= "") <*>
                         (v .:? "road" .!= "")
  parseJSON _ = undefined
