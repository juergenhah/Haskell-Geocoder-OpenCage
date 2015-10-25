{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

{-|
Module      : RevGeocoder
Description : Module to convert latitude and longitude coordinated into an address.
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

Requests the <http://geocoder.opencagedata.com> reverse geocoding service to transfer latitude and longitude values into an address.
-}
module RevGeocoder  (QueryParameters(..)
  ,defaultParams
  ,GeoCageDeveloperKey
  ,GeoCageLatitude
  ,GeoCageLongitude
  ,reverseGeocode
  ,reverseGeocodeWith
  ,getReverseAPIResponse
  ,getReverseAPIResponseWith
  ,ResponseBody(..)
  ,Result(..)
  ,License(..)
  ,Rate(..)
  ,Status(..)
  ,Timestamp(..)
  ,Components(..)
  ,Location(..)
  ,Annotations (..)) where
  
import HTTPWrapper
import qualified Data.Text as T (Text,unpack)
import GHC.Generics (Generic)
import Data.Aeson  hiding (Result) 
import Data.ByteString.Char8 as Bs (ByteString) 


-- * request

-- | data type including all possible parameters OpenCage API offers
data QueryParameters = QueryParameters {
  para_latitude::Double, -- ^ latitude value to reverse geocode
  para_longitude::Double, -- ^ longitude value to reverse geocode
  para_key::T.Text, -- ^ OpenCage key
  para_language:: Maybe T.Text, -- ^ language for the result: IETF format language code 
  para_pretty::Maybe T.Text, -- ^ should the result be pretty printed ?
  para_jsonp:: Maybe T.Text -- ^ should the result be included into a JSON function ?
}deriving (Show,Generic)

-- | minimal example of QueryParameters
-- can be used to create your own parameters
defaultParams :: QueryParameters
defaultParams = RevGeocoder.QueryParameters {
 para_latitude=0
 ,para_longitude=0
 ,para_key=""
 ,para_language=Just "en"
 ,para_pretty=Nothing
 ,para_jsonp=Nothing}
 
type GeoCageDeveloperKey = T.Text
type GeoCageLatitude = Double
type GeoCageLongitude = Double

-- | reverse geocode latitude and longitude, returns a list of Result values
reverseGeocode :: GeoCageLatitude -- ^ latitude value to reverse geocode
                -> GeoCageLongitude -- ^ longitude value to reverse geocode
                -> GeoCageDeveloperKey  -- ^ OpenCage key
                -> IO [Result] -- ^ list of results, empty if an error occured
reverseGeocode latt long developerKey = reverseGeocodeWith params 
 where params = defaultParams {para_latitude=latt, para_longitude=long,para_key=developerKey}

-- | reverse geocode passing all parameters via the QueryParameters type
reverseGeocodeWith :: QueryParameters -- ^ parameters for the request
                    -> IO [Result] -- ^ list of results, empty if an error occured
reverseGeocodeWith params = do
 r <- getReverseAPIResponseWith params
 case r of
  Nothing -> return []
  Just a -> return $ results a
  
-- | reverse geocode latitude and longitude, returns the whole Response
getReverseAPIResponse :: GeoCageLatitude -- ^ latitude value to reverse geocode
                        -> GeoCageLongitude -- ^ longitude value to reverse geocode
                        -> GeoCageDeveloperKey -- ^ OpenCage key
                        -> IO (Maybe ResponseBody) -- ^ if successful Just a ResponseBody, otherwise Nothing
getReverseAPIResponse latt long developerKey = getReverseAPIResponseWith params 
 where params = defaultParams {para_latitude=latt, para_longitude=long,para_key=developerKey}

-- | reverse geocode passing all parameters via the QueryParameters type
getReverseAPIResponseWith :: QueryParameters  -- ^ parameters for the request
                           -> IO (Maybe ResponseBody) -- ^ if successful Just a ResponseBody, otherwise Nothing
getReverseAPIResponseWith parameters =do
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


-- | converts QueryParameters to a list of tupels needed by the HTTPWrapper module
convertQParamstoOptionsList :: QueryParameters -- ^ data type including all request parameters 
                             -> [(String,String)] -- ^ list of tupels containing all request parameters
convertQParamstoOptionsList (QueryParameters para_lat para_long key lang pre jsonp )= [] ++
 [("q", show para_lat++","++show para_long),("key",T.unpack key)]
 ++ toParameter "language" lang 
 ++ toParameter "pretty" pre 
 ++ toParameter "jsonp" jsonp
 where toParameter :: String -> Maybe T.Text -> [(String,String)]
       toParameter param mValue = case mValue of
                                   Nothing -> []
                                   Just val -> [(param,T.unpack val)]

-- * response data types
 
-- | data type respecting OpenCage response encoded as JSON 
data ResponseBody = ResponseBody {licenses :: [License],
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
  url :: Url
} deriving (Show, Generic)
instance FromJSON License 

data Rate = Rate {
  limit :: Integer, 
  remaining::Integer, 
  reset::Double 
} deriving (Show, Generic)
instance FromJSON Rate

data Result = Result { 
  annotations :: Annotations, 
  components:: Components, 
  confidence:: Integer, 
  formatted::  T.Text , 
  geometry:: Location
} deriving (Show,Generic)

instance FromJSON Result where
  parseJSON (Object v) = Result    <$>
                         v .: "annotations" <*>
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

data Annotations = Annotations { 
  geohash::T.Text
} deriving (Show, Generic)
instance FromJSON Annotations

type Url = T.Text

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
  road ::  T.Text,
  clothes :: T.Text
} deriving (Show,Generic)

-- | not all components are included into the response, if so there is an empty string as default value.
instance FromJSON Components where 
  parseJSON (Object v) = Components    <$>
                         (v .:? "country" .!= "") <*>
                         (v .:? "country_code" .!= "") <*>
                         (v .:? "county" .!= "") <*>
                         (v .:? "state" .!= "") <*>
                         (v .:? "city" .!= "") <*>
                         (v .:? "road" .!= "") <*>
                         (v .:? "clothes" .!= "")
  parseJSON _ = undefined
