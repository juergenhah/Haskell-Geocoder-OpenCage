{-# LANGUAGE  DeriveGeneric,OverloadedStrings ,TemplateHaskell #-}


module GeoCage  (QueryParameters(..)
  ,defaultParams
  ,TokenToGeocode
  ,GeoCageDeveloperKey
  ,getAPIResponse
  ,getAPIResponseWith
  ,geocode
  ,geocodeWith
  ,geocodeGeoTokensList
  ,createGeocoderParams
  ,ResultBody(..)
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
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

import qualified Network.Wreq as Wreq
import Control.Lens ((&), (^.), (^?), (.~), (?~),makeLenses)
import qualified Control.Exception as E

import qualified Data.ByteString.Lazy as BS

type TokenToGeocode = T.Text
type GeoCageDeveloperKey = T.Text


data QueryParameters = QueryParameters {
  _para_q::T.Text,
  _para_key::T.Text,
  _para_language:: Maybe T.Text, -- IETF format language code 
  _para_pretty::Maybe T.Text,
  _para_bounds::Maybe T.Text, --southwest corner, northeast corner
  _para_country::Maybe T.Text, -- ISO 3166-1 Alpha 3
  _para_fields::Maybe T.Text --comma separated list
}deriving (Show,Generic)
$(makeLenses ''QueryParameters)

defaultParams = QueryParameters {
  _para_q="",
  _para_key="",
  _para_language=Just "en",
  _para_pretty=Nothing,
  _para_bounds=Nothing,
  _para_country=Nothing,
  _para_fields=Nothing}

getAPIResponse :: TokenToGeocode -> GeoCageDeveloperKey -> IO (Maybe ResultBody)
getAPIResponse tokenToGeocode developerKey = 
  getAPIResponseWith parameters
  where parameters = defaultParams {_para_q= tokenToGeocode,_para_key=developerKey}

getAPIResponseWith :: GeoCage.QueryParameters -> IO (Maybe ResultBody)
getAPIResponseWith parameters =do 
  let httpopts = Wreq.defaults  
             & Wreq.param "q" .~ [_para_q parameters] 
             & Wreq.param "key" .~ [_para_key parameters] 
             & maybe id (\p -> Wreq.param "language" .~ [p]) 
               (_para_language parameters)
             & maybe id (\p -> Wreq.param "pretty" .~ [p]) 
               (_para_pretty parameters)
             & maybe id (\p -> Wreq.param "bounds" .~ [p]) 
               (_para_bounds parameters)
             & maybe id (\p -> Wreq.param "country" .~ [p]) 
               (_para_country parameters)
             & maybe id (\p -> Wreq.param "fields" .~ [p]) 
               (_para_fields parameters)
  -- send the HTTP Request
  wasHTTPRequestsuccesfull <- E.try (Wreq.getWith httpopts "http://api.opencagedata.com/geocode/v1/json?")  :: IO (Either E.SomeException (Wreq.Response BS.ByteString))
  case wasHTTPRequestsuccesfull of
   Left exception -> do putStrLn "exception at HTTP request to opencagedata, issue: "
                        print exception
                        return Nothing
   Right response -> do -- convert JSON into the ResultBody data type
     let conversionJSONsucc = Wreq.asJSON response :: Either E.SomeException (Wreq.Response ResultBody)
     case conversionJSONsucc of
              Left exception -> do putStrLn "exception at ByteString to JSON conversion, issue:"
                                   print exception
                                   return Nothing
              Right resultBody -> do
                      return $ Just (resultBody ^. Wreq.responseBody)

geocode :: TokenToGeocode-> GeoCageDeveloperKey -> IO [GeoCage.Result]
geocode tokentoGeocode developerKey = geocodeWith params
  where params = defaultParams {_para_q=tokentoGeocode,_para_key=developerKey}
  
-- returns an empty list if an exception occured or the result was empty
geocodeWith :: GeoCage.QueryParameters ->  IO [GeoCage.Result]
geocodeWith params = do 
  resp <- getAPIResponseWith params 
  case resp of
   Nothing -> return []
   Just r -> return $ GeoCage.results r

geocodeGeoTokensList :: [TokenToGeocode]-> GeoCageDeveloperKey  -> IO()
geocodeGeoTokensList geoTokens developerKey = do
 let geocodeparams = GeoCage.defaultParams {GeoCage._para_key=developerKey,
                                               GeoCage._para_language= Just$ T.pack  "en",
                                               GeoCage._para_country = Just $ T.pack "DEU"}
 let params =  createGeocoderParams geocodeparams geoTokens
 results<-  mapM geocodeWith params
 print results

createGeocoderParams :: QueryParameters -> [TokenToGeocode]-> [QueryParameters]
createGeocoderParams params geoTokens = zipWith (\p t -> p {_para_q=t}) 
                   (replicate (length geoTokens) params) 
                   geoTokens

-- data types for the geocoding result 
data ResultBody = ResultBody { 
  licenses :: [License],
  rate :: Rate,
  results :: [Result],
  status :: Status,
  thanks :: T.Text,
  timestamp :: Timestamp,
  total_results :: Integer,
  we_are_hiring :: T.Text 
} deriving (Show,Generic)
instance FromJSON ResultBody
  
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

-- empty string if not present
instance FromJSON Components where 
  parseJSON (Object v) = Components    <$>
                         (v .:? "country" .!= "") <*>
                         (v .:? "country_code" .!= "") <*>
                         (v .:? "county" .!= "") <*>
                         (v .:? "state" .!= "") <*>
                         (v .:? "city" .!= "") <*>
                         (v .:? "road" .!= "")
