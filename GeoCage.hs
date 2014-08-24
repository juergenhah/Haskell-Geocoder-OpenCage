{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}


-- | Add documentation here
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
import Data.ByteString.Char8 as Bs (ByteString) 

import HTTPWrapper
type TokenToGeocode = T.Text
type GeoCageDeveloperKey = T.Text


-- | Add documentation here
data QueryParameters = QueryParameters {
  para_q::T.Text,
  para_key::T.Text,
  para_language:: Maybe T.Text, -- IETF format language code 
  para_pretty::Maybe T.Text,
  para_bounds::Maybe T.Text, --southwest corner, northeast corner
  para_country::Maybe T.Text, -- ISO 3166-1 Alpha 3
  para_fields::Maybe T.Text --comma separated list
}deriving (Show,Generic)


-- | Add documentation here
defaultParams :: QueryParameters
defaultParams = QueryParameters {
  para_q="",
  para_key="",
  para_language=Just "en",
  para_pretty=Nothing,
  para_bounds=Nothing,
  para_country=Nothing,
  para_fields=Nothing}

-- | Add documentation here
getAPIResponse :: TokenToGeocode -> GeoCageDeveloperKey -> IO (Maybe ResultBody)
getAPIResponse tokenToGeocode developerKey = 
  getAPIResponseWith parameters
  where parameters = defaultParams {para_q= tokenToGeocode,para_key=developerKey}


-- | Add documentation here
getAPIResponseWith :: GeoCage.QueryParameters -> IO (Maybe ResultBody)
getAPIResponseWith parameters =do
  reqSuccesfull <-requestGeocage parameters
  case reqSuccesfull of
    Left err -> do   putStrLn "exception at geoCage request, issue:"
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
 
-- | Add documentation here
convertQParamstoOptionsList :: QueryParameters -> [(String,String)]
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
convertQParamstoOptionsList _ = []


-- | Add documentation here
geocode :: TokenToGeocode-> GeoCageDeveloperKey -> IO [GeoCage.Result]
geocode tokentoGeocode developerKey = geocodeWith params
  where params = defaultParams {para_q=tokentoGeocode,para_key=developerKey}
  

-- | returns an empty list if an exception occured or the result was empty
geocodeWith :: GeoCage.QueryParameters ->  IO [GeoCage.Result]
geocodeWith params = do 
  resp <- getAPIResponseWith params 
  case resp of
   Nothing -> return []
   Just r -> return $ GeoCage.results r

-- | Add documentation here
geocodeGeoTokensList :: [TokenToGeocode]-> GeoCageDeveloperKey  -> IO()
geocodeGeoTokensList geoTokens developerKey = do
 let geocodeparams = GeoCage.defaultParams {GeoCage.para_key=developerKey,
                                               GeoCage.para_language= Just$ T.pack  "en",
                                               GeoCage.para_country = Just $ T.pack "DEU"}
 let params =  createGeocoderParams geocodeparams geoTokens
 res<-  mapM geocodeWith params
 print res

-- | Add documentation here
createGeocoderParams :: QueryParameters -> [TokenToGeocode]-> [QueryParameters]
createGeocoderParams params geoTokens = zipWith (\p t -> p {para_q=t}) 
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

-- empty string if not present
instance FromJSON Components where 
  parseJSON (Object v) = Components    <$>
                         (v .:? "country" .!= "") <*>
                         (v .:? "country_code" .!= "") <*>
                         (v .:? "county" .!= "") <*>
                         (v .:? "state" .!= "") <*>
                         (v .:? "city" .!= "") <*>
                         (v .:? "road" .!= "")
  parseJSON _ = undefined