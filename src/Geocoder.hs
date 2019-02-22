{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

{-|
Module      : Geocoder
Description : Module to convert an address into coordinates.
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

Requests the <http://geocoder.opencagedata.com> geocoding service to get  latitude and longitude values from an address. Uses Aeson to convert the response from JSON to Haskell types.
-}
module Geocoder
  ( QueryParameters(..)
  , defaultParams
  , TokenToGeocode
  , GeoCageDeveloperKey
  , getAPIResponse
  , getAPIResponseWith
  , geocode
  , geocodeWith
  , geocodeGeoTokensList
  , createGeocoderParams
  , ResponseBody(..)
  , Result(..)
  , License(..)
  , Rate(..)
  , Status(..)
  , Timestamp(..)
  , Components(..)
  , Location(..)
  , Bounds(..)
  ) where

import Data.Aeson hiding (Result)
import Data.List (intercalate)
import qualified Data.Text as T (Text, intercalate, pack, unpack)
import GHC.Generics (Generic)

import Data.ByteString.Char8 as Bs (ByteString)

import HTTPWrapper

-- * Request Type
-- | data type including all possible parameters OpenCage API offers
data QueryParameters = QueryParameters
  { para_abbreviate :: Bool -- ^ shortten the formatted string
  , para_addRequest :: Bool -- ^ Return parameters in the response
  , para_bounds :: Maybe (Float, Float, Float, Float) -- ^ rectangle hint: southwest corner, northeast corner
  , para_country :: [T.Text] -- ^ country hint:  ISO 3166-1 Alpha 3
  , para_fields :: Maybe T.Text -- ^ comma separated list
  , para_key :: T.Text -- ^ OpenCage key
  , para_language :: Maybe T.Text -- ^ language for the result: IETF format language code 
  , para_limit :: Maybe Integer -- ^ number of results to return
  , para_minConfidence :: Maybe Integer -- ^ cutoff on confidence results
  , para_noAnnotations :: Bool
  , para_noDedupe :: Bool
  , para_noRecord :: Bool
  , para_pretty :: Maybe T.Text -- ^ should the result be pretty printed ?
  , para_proximity :: Maybe (Float, Float) -- ^ Only use in forward geocoding. Bias results based on proximity
  , para_q :: T.Text -- ^ Text to query
  } deriving (Show)

-- | minimal example of QueryParameters
-- can be used to create your own parameters
defaultParams :: QueryParameters
defaultParams =
  QueryParameters
    { para_abbreviate = False
    , para_addRequest = False
    , para_bounds = Nothing
    , para_country = []
    , para_fields = Nothing
    , para_key = ""
    , para_language = Just "en"
    , para_limit = Nothing
    , para_minConfidence = Nothing
    , para_noAnnotations = True
    , para_noDedupe = False
    , para_noRecord = False
    , para_pretty = Nothing
    , para_proximity = Nothing
    , para_q = ""
    }

type TokenToGeocode = T.Text

type GeoCageDeveloperKey = T.Text

-- * Request functions
-- | Minimalistic version to geocode a token returning the whole Response
getAPIResponse ::
     TokenToGeocode -- ^ token to geocode
  -> GeoCageDeveloperKey -- ^ OpenCage key
  -> IO (Maybe ResponseBody) -- ^ if the geocoding was successful there is a ResultBody full of data returned
getAPIResponse tokenToGeocode developerKey = getAPIResponseWith parameters
  where
    parameters =
      defaultParams {para_q = tokenToGeocode, para_key = developerKey}

-- | main geocode function, called by all other functions, deals with errors 
getAPIResponseWith ::
     QueryParameters -- ^ parameters included into the request
  -> IO (Maybe ResponseBody) -- ^ if the geocoding was successful there is a ResultBody full of data returned
getAPIResponseWith parameters = do
  reqSuccesfull <- requestGeocage parameters
  case reqSuccesfull of
    Left err -> do
      putStrLn "exception at OpenCage request, issue:"
      print err
      return Nothing
    Right response ->
      case eitherDecodeStrict response of
        Left exception -> do
          putStrLn "exception at ByteString to JSON conversion, issue:"
          print exception
          return Nothing
        Right resultBody -> return $ Just resultBody
  where
    requestGeocage :: QueryParameters -> IO (Either String ByteString)
    requestGeocage params = download geocageUrl para
      where
        geocageUrl = "http://api.opencagedata.com/geocode/v1/json?"
        para = convertQParamstoOptionsList params

-- | converts QueryParameters to a list of tupels needed by the HTTPWrapper module 
convertQParamstoOptionsList ::
     QueryParameters -- ^ data type including all request parameters 
  -> [(String, String)] -- ^ list of tupels containing all request parameters
convertQParamstoOptionsList QueryParameters {..} =
  [("q", T.unpack para_q), ("key", T.unpack para_key)] ++
  bool2Param "abbrv" para_abbreviate ++
  bool2Param "add_request" para_addRequest ++
  maybe2Param "bounds" coords2Param para_bounds ++
  [("country", T.unpack (T.intercalate "," para_country))] ++
  maybe2Param "fields" T.unpack para_fields ++
  maybe2Param "language" T.unpack para_language ++
  maybe2Param "limit" show para_limit ++
  maybe2Param "min_confidence" show para_minConfidence ++
  bool2Param "no_annotations" para_noAnnotations ++
  bool2Param "no_dedupe" para_noDedupe ++
  bool2Param "no_record" para_noRecord ++
  maybe2Param "pretty" T.unpack para_pretty ++
  maybe2Param "proximity" point2Param para_proximity
  where
    point2Param :: (Float, Float) -> String
    point2Param (x, y) = intercalate "," (map show [x, y])
    coords2Param :: (Float, Float, Float, Float) -> String
    coords2Param (south, west, north, east) =
      intercalate "," (map show [south, west, north, east])
    bool2Param :: String -> Bool -> [(String, String)]
    bool2Param param bValue = [(param, "1") | bValue]
    maybe2Param :: String -> (a -> String) -> Maybe a -> [(String, String)]
    maybe2Param param transform mValue =
      case mValue of
        Nothing -> []
        Just val -> [(param, transform val)]

-- | geocodes the token and returns a list of results
geocode ::
     TokenToGeocode -- ^ token to geocode, empty if an error occured
  -> GeoCageDeveloperKey -- ^ OpenCage key
  -> IO [Result] -- ^ list of results
geocode tokentoGeocode developerKey = geocodeWith params
  where
    params = defaultParams {para_q = tokentoGeocode, para_key = developerKey}

-- | geocodes the token included into the QueryParameters respecting all values set in the QueryParameters 
-- returns an empty list if an exception occured or the result was empty
geocodeWith ::
     QueryParameters -- ^ parameters included into the request
  -> IO [Result] -- ^ list of results, empty if an error occured
geocodeWith params = do
  resp <- getAPIResponseWith params
  case resp of
    Nothing -> return []
    Just r -> return $ results r

-- | geocodes a list of tokens and prints the results
geocodeGeoTokensList ::
     [TokenToGeocode] -- ^ list of tokens to geocode
  -> GeoCageDeveloperKey -- ^ OpenCage key
  -> IO () -- ^ prints the result
geocodeGeoTokensList geoTokens developerKey = do
  let geocodeparams =
        defaultParams
          { para_key = developerKey
          , para_language = Just $ T.pack "en"
          , para_country = [T.pack "DEU"]
          }
  let params = createGeocoderParams geocodeparams geoTokens
  res <- mapM geocodeWith params
  print res

-- | packs tokens into QueryParameters and returns this as a list
-- if you want to specify your own parameters use this function
createGeocoderParams ::
     QueryParameters -- ^ parameters that should be used for all tokens in the list
  -> [TokenToGeocode] -- ^ list of tokens to geocode
  -> [QueryParameters] -- ^ list of parameters, for every token one QueryParameter entry is included
createGeocoderParams params geoTokens =
  zipWith
    (\p t -> p {para_q = t})
    (replicate (length geoTokens) params)
    geoTokens

-- * data types for the geocoding result
-- | data type respecting OpenCage response encoded as JSON 
data ResponseBody = ResponseBody
  { documentation :: String
  , licenses :: [License]
  , rate :: Rate
  , results :: [Result]
  , status :: Status
  , thanks :: T.Text
  , timestamp :: Timestamp
  , total_results :: Integer
  } deriving (Show, Generic)

instance FromJSON ResponseBody

data License = License
  { name :: T.Text
  , url :: T.Text
  } deriving (Show, Generic)

instance FromJSON License

data Rate = Rate
  { limit :: Integer
  , remaining :: Integer
  , reset :: Integer
  } deriving (Show, Generic)

instance FromJSON Rate

data Currency = Currency
  { alternateSymbols :: Maybe [String]
  , decimalMark :: Maybe String
  , htmlEntity :: Maybe String
  , isoCode :: Maybe String
  , isoNumeric :: Maybe Integer
  , _name :: Maybe String
  , smallestDenominator :: Maybe Integer
  , subunit :: Maybe String
  , subunitToUnit :: Maybe Integer
  , symbol :: Maybe String
  , symbolFirst :: Maybe Integer
  , thousandsSeparator :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Currency

data DMS = DMS
  { dms_lat :: Double
  , dms_lng :: Double
  } deriving (Show, Generic)

instance FromJSON DMS where
  parseJSON =
    withObject "dms" $ \o -> do
      dms_lat <- o .: "lat"
      dms_lng <- o .: "lng"
      return DMS {..}

data OSM = OSM
  { osm_editUrl :: String
  , osm_url :: String
  } deriving (Show, Generic)

instance FromJSON OSM where
  parseJSON =
    withObject "oms" $ \o -> do
      osm_editUrl <- o .: "edit_url"
      osm_url <- o .: "url"
      return OSM {..}

data MercatorProjection = MercatorProjection
  { x :: Float
  , y :: Float
  } deriving (Show, Generic)

instance FromJSON MercatorProjection

data What3Words = What3Words
  { words :: String
  } deriving (Show, Generic)

instance FromJSON What3Words

data SunTimings = SunTimings
  { apparent :: Integer
  , astronomical :: Integer
  , civil :: Integer
  , nautical :: Integer
  } deriving (Show, Generic)

instance FromJSON SunTimings

data Sun = Sun
  { rise :: SunTimings
  , set :: SunTimings
  } deriving (Show, Generic)

instance FromJSON Sun

data Timezone = Timezone
  { tz_name :: String
  , nowInDst :: Bool
  , offsetSec :: Int
  , offsetString :: Int
  , shortName :: String
  } deriving (Show, Generic)

instance FromJSON Timezone where
  parseJSON =
    withObject "timezone" $ \o -> do
      tz_name <- o .: "name"
      nowInDst <- o .: "now_in_dst"
      offsetSec <- o .: "offset_sec"
      offsetString <- o .: "offset_string"
      shortName <- o .: "short_name"
      return Timezone {..}

data Annotations = Annotations
  { currency :: Maybe Currency
  , dms :: Maybe DMS
  , mgrs :: Maybe String
  , osm :: Maybe OSM
  , maidenhead :: Maybe String
  , flag :: Maybe String
  , mercator :: Maybe MercatorProjection
  , callingCode :: Maybe Int
  , geohash :: Maybe String
  , qibla :: Maybe Float
  , wikidata :: Maybe String
  , what3Words :: Maybe What3Words
  , sun :: Maybe Sun
  , timezone :: Maybe Timezone
  } deriving (Show, Generic)

instance FromJSON Annotations where
  parseJSON =
    withObject "annotations" $ \o -> do
      currency <- o .:? "currency"
      dms <- o .:? "DMS"
      mgrs <- o .:? "MGRS"
      osm <- o .:? "OSM"
      maidenhead <- o .:? "Maidenhead"
      flag <- o .:? "flag"
      mercator <- o .:? "Mercator"
      callingCode <- o .:? "callingcode"
      geohash <- o .:? "geohash"
      qibla <- o .:? "qibla"
      wikidata <- o .:? "wikidata"
      what3Words <- o .:? "what2words"
      sun <- o .:? "sun"
      timezone <- o .:? "timezone"
      return Annotations {..}

data Result = Result
  { annotations :: Maybe Annotations
  , bounds :: Bounds
  , components :: Components
  , confidence :: Integer
  , formatted :: T.Text
  , geometry :: Location
  } deriving (Show, Generic)

instance FromJSON Result where
  parseJSON (Object v) =
    Result <$> v .:? "annotations" <*> v .: "bounds" <*> v .: "components" <*>
    v .: "confidence" <*>
    v .:? "formatted" .!= "" <*>
    v .: "geometry"
  parseJSON _ = undefined

data Status = Status
  { code :: Integer
  , message :: T.Text
  } deriving (Show, Generic)

instance FromJSON Status

data Timestamp = Timestamp
  { created_http :: T.Text
  , created_unix :: Integer
  } deriving (Show, Generic)

instance FromJSON Timestamp

data Bounds = Bounds
  { northeast :: Location
  , southwest :: Location
  } deriving (Show, Generic)

instance FromJSON Bounds

data Location = Location
  { lat :: Double
  , lng :: Double
  } deriving (Show, Generic)

instance FromJSON Location

data Components = Components
  { iso_3166_1_alpha_2 :: T.Text
  , iso_3166_1_alpha_3 :: T.Text
  , country :: T.Text
  , country_code :: T.Text
  , county :: T.Text
  , state :: T.Text
  , city :: T.Text
  , suburb :: T.Text
  , road :: T.Text
  } deriving (Show, Generic)

-- | not all components are included into the response, if so there is an empty string as default value
instance FromJSON Components where
  parseJSON (Object v) =
    Components <$> (v .:? "ISO_3166-1_alpha-2" .!= "") <*>
    (v .:? "ISO_3166-1_alpha-3" .!= "") <*>
    (v .:? "country" .!= "") <*>
    (v .:? "country_code" .!= "") <*>
    (v .:? "county" .!= "") <*>
    (v .:? "state" .!= "") <*>
    (v .:? "city" .!= "") <*>
    (v .:? "suburb" .!= "") <*>
    (v .:? "road" .!= "")
  parseJSON _ = undefined
