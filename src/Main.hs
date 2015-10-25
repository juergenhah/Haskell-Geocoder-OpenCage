{-|
Module      : Main
Description : Some examples howto use the Geocoder and RevGeocoder modules.
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

Examples to show how Geocoder and RevGeocoder works.
-}
module Main where

import qualified Geocoder    as Geocoder
import qualified RevGeocoder as RevGeocoder

import qualified Data.Text   as T (Text (), pack, unpack)

main :: IO()
main = do
  putStrLn "geocode NewYork:"
  _ <- geocodeNewYork
  putStrLn "geocode Vienna"
  _ <- geocodeVienna
  putStrLn "geocode Vienna"
  _ <- geocodeVienna
  putStrLn "geocode Austria show results in German"
  geocodeAustriaGermanResult
  putStrLn "reverse geocode a TU building"
  reverseGeocodeTUVienna 
  putStrLn "reverse geocode a TU building"
  testGeocodeAPI1

-- | function for your OpenCage key
myDeveloperkey :: T.Text
myDeveloperkey = T.pack "put your OpenCage key here"
-- * geocoding

-- | requests the OpenCage Geocoder to geocode the token: Now York and returns the whole result body
--
-- >>> geocodeNewYork
-- Just (ResponseBody {licenses = [License {name = "CC-BY-SA", url = "http://creativecommons.org/licenses/by-sa/3.0/"},License {name = "ODbL", url = "http://opendatacommons.org/licenses/odbl/summary/"}], rate = Rate {limit = 2500, remaining = 2463, reset = 1.4458176e9}, results = [Result {bounds = Bounds {northeast = Location {lat = 40.917577, lng = -73.7001714}, southwest = Location {lat = 40.477399, lng = -74.2590899}}, components = Components {country = "United States of America", country_code = "us", county = "", state = "New York", city = "New York City", road = ""}, confidence = 2, formatted = "New York City, United States of America", geometry = Location {lat = 40.7305991, lng = -73.9865812}},Result {bounds = Bounds {northeast = Location {lat = 45.0158611, lng = -71.7897328}, southwest = Location {lat = 40.477399, lng = -79.7623534}}, components = Components {country = "United States of America", country_code = "us", county = "", state = "New York", city = "", road = ""}, confidence = 1, formatted = "New York, United States of America", geometry = Location {lat = 43.1561681, lng = -75.8449946}},Result {bounds = Bounds {northeast = Location {lat = 41.4047714, lng = -73.340764}, southwest = Location {lat = 40.1247714, lng = -74.620764}}, components = Components {country = "United States of America", country_code = "us", county = "New York", state = "New York", city = "New York City", road = ""}, confidence = 1, formatted = "New York City, New York, United States of America", geometry = Location {lat = 40.7647714, lng = -73.980764}},Result {bounds = Bounds {northeast = Location {lat = 55.0452998, lng = -1.4669496}, southwest = Location {lat = 55.0052998, lng = -1.5069496}}, components = Components {country = "United Kingdom", country_code = "gb", county = "Tyne and Wear", state = "England", city = "", road = ""}, confidence = 7, formatted = "New York, Tyne and Wear, England, United Kingdom", geometry = Location {lat = 55.0252998, lng = -1.4869496}},Result {bounds = Bounds {northeast = Location {lat = 30.8585202, lng = -87.1808048}, southwest = Location {lat = 30.8185202, lng = -87.2208048}}, components = Components {country = "United States of America", country_code = "us", county = "Santa Rosa County", state = "Florida", city = "", road = ""}, confidence = 7, formatted = "New York, Santa Rosa County, Florida, United States of America", geometry = Location {lat = 30.8385202, lng = -87.2008048}},Result {bounds = Bounds {northeast = Location {lat = 39.7052874, lng = -93.9068836}, southwest = Location {lat = 39.6652874, lng = -93.9468836}}, components = Components {country = "United States of America", country_code = "us", county = "Caldwell County", state = "Missouri", city = "", road = ""}, confidence = 7, formatted = "New York, Caldwell County, Missouri, United States of America", geometry = Location {lat = 39.6852874, lng = -93.9268836}},Result {bounds = Bounds {northeast = Location {lat = 40.8716701, lng = -93.2399318}, southwest = Location {lat = 40.8316701, lng = -93.2799318}}, components = Components {country = "United States of America", country_code = "us", county = "Wayne County", state = "Iowa", city = "", road = ""}, confidence = 7, formatted = "New York, Wayne County, Iowa, United States of America", geometry = Location {lat = 40.8516701, lng = -93.2599318}},Result {bounds = Bounds {northeast = Location {lat = 37.0089428, lng = -88.9325629}, southwest = Location {lat = 36.9689428, lng = -88.9725629}}, components = Components {country = "United States of America", country_code = "us", county = "Ballard County", state = "Kentucky", city = "", road = ""}, confidence = 7, formatted = "New York, Ballard County, Kentucky, United States of America", geometry = Location {lat = 36.9889428, lng = -88.9525629}},Result {bounds = Bounds {northeast = Location {lat = 35.0786534, lng = -107.5072717}, southwest = Location {lat = 35.0386534, lng = -107.5472717}}, components = Components {country = "United States of America", country_code = "us", county = "Cibola County", state = "New Mexico", city = "", road = ""}, confidence = 7, formatted = "New York, Cibola County, New Mexico, United States of America", geometry = Location {lat = 35.0586534, lng = -107.5272717}},Result {bounds = Bounds {northeast = Location {lat = 32.1879321, lng = -95.6491277}, southwest = Location {lat = 32.1479321, lng = -95.6891277}}, components = Components {country = "United States of America", country_code = "us", county = "Henderson County", state = "Texas", city = "", road = ""}, confidence = 7, formatted = "New York, Henderson County, Texas, United States of America", geometry = Location {lat = 32.1679321, lng = -95.6691277}}], status = Status {code = 200, message = "OK"}, thanks = "For using an OpenCage Data API", timestamp = Timestamp {created_http = "Sun, 25 Oct 2015 11:23:04 GMT", created_unix = 1445772184}, total_results = 10})
geocodeNewYork :: IO (Maybe Geocoder.ResponseBody) -- ^ Nothing if the request was not successful, Just the ResultBody
geocodeNewYork = Geocoder.getAPIResponse (T.pack "New York") myDeveloperkey


-- | requests the String Delhi and returns only the license part of the response
--
-- >>> checkLicensesforDelhi
-- which licenses are used:
-- [License {name = "CC-BY-SA", url = "http://creativecommons.org/licenses/by-sa/3.0/"},License {name = "ODbL", url = "http://opendatacommons.org/licenses/odbl/summary/"}]
checkLicensesforDelhi :: IO () -- ^ prints the results
checkLicensesforDelhi = do
  let params = Geocoder.defaultParams {Geocoder.para_q = T.pack "Delhi",Geocoder.para_key=myDeveloperkey}
  eitherResponse<-Geocoder.getAPIResponseWith params
  case eitherResponse of
    Nothing -> putStrLn "no result delivered from the geocoder"
    Just r -> do
      putStrLn "which licenses are used:"
      print (Geocoder.licenses r)

-- | requests an ordinary state (e.g: Delhi) and returns only the number of remaining requests OpenCage allows
--
-- >>> howManyRequestsLeft
-- 2062
howManyRequestsLeft :: IO Integer -- ^ number of remaining requests OpenCage allows for the actual period   
howManyRequestsLeft = do
  let params = Geocoder.defaultParams {Geocoder.para_q = T.pack "Delhi",Geocoder.para_key=myDeveloperkey}
  eitherResponse<-Geocoder.getAPIResponseWith params
  case eitherResponse of
    Nothing -> return (-1)
    Just r -> return (Geocoder.remaining . Geocoder.rate $ r)

-- | geocodes Vienna and returns only the results of the responseBody
--
-- >>> geocodeVienna
-- [Result {bounds = Bounds {northeast = Location {lat = 48.3683537, lng = 16.5325042}, southwest = Location {lat = 48.0483537, lng = 16.2125042}}, components = Components {country = "Austria", country_code = "at", county = "", state = "Vienna", city = "Vienna", road = ""}, confidence = 3, formatted = "Vienna, Austria", geometry = Location {lat = 48.2083537, lng = 16.3725042}},Result {bounds = Bounds {northeast = Location {lat = 47.1759223, lng = 1.2131909}, southwest = Location {lat = 46.0482926, lng = -0.1049762}}, components = Components {country = "France", country_code = "fr", county = "Vienne", state = "Poitou-Charentes", city = "", road = ""}, confidence = 1, formatted = "Vienne, France", geometry = Location {lat = 46.61211655, lng = 0.465407009663971}},Result {bounds = Bounds {northeast = Location {lat = 48.2184891, lng = 16.3849175}, southwest = Location {lat = 48.1995278, lng = 16.3552089}}, components = Components {country = "Austria", country_code = "at", county = "", state = "Vienna", city = "Vienna", road = ""}, confidence = 7, formatted = "1010 Vienna, Austria", geometry = Location {lat = 48.2091332, lng = 16.3699915}},Result {bounds = Bounds {northeast = Location {lat = 38.921824, lng = -77.2412649}, southwest = Location {lat = 38.878494, lng = -77.2847629}}, components = Components {country = "United States of America", country_code = "us", county = "Fairfax County", state = "Virginia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Fairfax County, Virginia, United States of America", geometry = Location {lat = 38.9013013, lng = -77.2651585}},Result {bounds = Bounds {northeast = Location {lat = 48.8309113, lng = 21.9877973}, southwest = Location {lat = 48.7909113, lng = 21.9477973}}, components = Components {country = "Slovakia", country_code = "sk", county = "", state = "Region of Ko\353ice", city = "", road = ""}, confidence = 7, formatted = "072 31 Vinn\233, Slovakia", geometry = Location {lat = 48.8109113, lng = 21.9677973}},Result {bounds = Bounds {northeast = Location {lat = 38.197853, lng = -91.9348889}, southwest = Location {lat = 38.175516, lng = -91.9629559}}, components = Components {country = "United States of America", country_code = "us", county = "Maries County", state = "Missouri", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Maries County, Missouri, United States of America", geometry = Location {lat = 38.1867085, lng = -91.947112}},Result {bounds = Bounds {northeast = Location {lat = 39.348538, lng = -81.5073038}, southwest = Location {lat = 39.296021, lng = -81.5564408}}, components = Components {country = "United States of America", country_code = "us", county = "Wood County", state = "West Virginia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Wood County, West Virginia, United States of America", geometry = Location {lat = 39.3270191, lng = -81.5484578}},Result {bounds = Bounds {northeast = Location {lat = 37.4034172, lng = -121.9985449}, southwest = Location {lat = 37.4033172, lng = -121.9986449}}, components = Components {country = "United States of America", country_code = "us", county = "Santa Clara County", state = "California", city = "Sunnyvale", road = "West Tasman Drive"}, confidence = 10, formatted = "Vienna, West Tasman Drive, Sunnyvale, CA 94089, United States of America", geometry = Location {lat = 37.4033672, lng = -121.9985949}},Result {bounds = Bounds {northeast = Location {lat = 32.112459, lng = -83.7615469}, southwest = Location {lat = 32.066248, lng = -83.8138599}}, components = Components {country = "United States of America", country_code = "us", county = "Dooly County", state = "Georgia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Dooly County, Georgia, United States of America", geometry = Location {lat = 32.0913402, lng = -83.7964675}}]
geocodeVienna :: IO [Geocoder.Result]
geocodeVienna = Geocoder.geocode (T.pack "Vienna") myDeveloperkey

-- | geocodes Vienna where the result should be converted into german
--
-- >>> geocodeAustriaGermanResult
-- Österreich
-- Dubai, Vereinigte Arabische Emirate
-- Austria, Recoleta, C1118AAT Buenos Aires, Argentinien
-- Austria, Recoleta, C1425BGR Buenos Aires, Argentinien
-- Austria, Recoleta, C1129ABO Buenos Aires, Argentinien
-- Austria, La Carolina, EC170515, Quito, Ecuador
-- Austria, Lima L05, Peru
-- Austria, Coquimbo, Chile
-- Austria, 12598 Peníscola / Peñíscola, Spanien
-- Austria, Real del Bosque, 91096 Xalapa, Veracruz de Ignacio de la Llave, Mexiko
geocodeAustriaGermanResult :: IO ()
geocodeAustriaGermanResult= do
  let params= Geocoder.defaultParams {Geocoder.para_q = T.pack "Austria"
                                    ,Geocoder.para_key=myDeveloperkey
                                    ,Geocoder.para_language= Just $  T.pack "de"}
  results <- Geocoder.geocodeWith params
  let strResults= map (T.unpack.Geocoder.formatted) results
  mapM_ putStrLn strResults

-- | geocodes a list of Strings and prints the result
testGeocodeAPI1 :: IO ()
testGeocodeAPI1 = do
  let tokens = map T.pack ["Berlin","München","Tokyo","invalid","San Francisco","White House"]
  Geocoder.geocodeGeoTokensList tokens myDeveloperkey

-- * reverse geocoding

-- | gets the address from the coordinates (Vienna University of Technology)
reverseGeocodeTUVienna :: IO ()
reverseGeocodeTUVienna = do
    results<-RevGeocoder.reverseGeocode 48.19637 16.36992 myDeveloperkey
    let formattedResult= map (T.unpack.RevGeocoder.formatted) results
    mapM_ putStrLn formattedResult

-- | a request using RevGeocoder.defaultParams
reverseGeocodeTest1 :: IO (Maybe RevGeocoder.ResponseBody)
reverseGeocodeTest1 = RevGeocoder.getReverseAPIResponseWith revGeocodeParams
 where revGeocodeParams = RevGeocoder.defaultParams {RevGeocoder.para_latitude=(-22.6792)::Double,
  RevGeocoder.para_longitude=(14.5272)::Double,RevGeocoder.para_key=myDeveloperkey}
