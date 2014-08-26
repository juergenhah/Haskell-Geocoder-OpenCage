{-|
Module      : GeoCageExamples
Description : Some examples howto use the Geocoder and RevGeocoder modules.
Copyright   : 
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental

Examples to show how Geocoder and RevGeocoder works.
-}
module GeoCageExamples where

import qualified Geocoder    as Geocoder
import qualified RevGeocoder as RevGeocoder

import qualified Data.Text   as T (Text (), pack, unpack)

-- | function for your OpenCage key
myDeveloperkey :: T.Text
myDeveloperkey = T.pack "put your OpenCage key here"

-- * geocoding

-- | requests the OpenCage Geocoder to geocode the token: Now York and returns the whole result body
--
-- >>> geocodeNewYork
-- Just (ResultBody {licenses = [License {name = "CC-BY-SA", url = "http://creativecommons.org/licenses/by-sa/3.0/"},License {name = "ODbL", url = "http://opendatacommons.org/licenses/odbl/summary/"}], rate = Rate {limit = 2500, remaining = 2474, reset = 1.4090976e9}, results = [Result {bounds = Bounds {northeast = Location {lat = 40.917577, lng = -73.7001714}, southwest = Location {lat = 40.477399, lng = -74.2590899}}, components = Components {country = "United States of America", country_code = "US", county = "", state = "New York", city = "New York City", road = ""}, confidence = 2, formatted = "New York City, New York City, New York, United States of America", geometry = Location {lat = 40.7305991, lng = -73.9865812}},Result {bounds = Bounds {northeast = Location {lat = 45.0158611, lng = -71.8527116}, southwest = Location {lat = 40.477399, lng = -79.76251}}, components = Components {country = "United States of America", country_code = "US", county = "", state = "New York", city = "", road = ""}, confidence = 1, formatted = "New York, United States of America", geometry = Location {lat = 43.1561681, lng = -75.8449946}},Result {bounds = Bounds {northeast = Location {lat = 41.4047714, lng = -73.340764}, southwest = Location {lat = 40.1247714, lng = -74.620764}}, components = Components {country = "United States of America", country_code = "US", county = "New York", state = "New York", city = "", road = ""}, confidence = 1, formatted = "New York, New York City, New York, United States of America", geometry = Location {lat = 40.7647714, lng = -73.980764}},Result {bounds = Bounds {northeast = Location {lat = 55.0452998, lng = -1.4669496}, southwest = Location {lat = 55.0052998, lng = -1.5069496}}, components = Components {country = "United Kingdom", country_code = "gb", county = "Tyne and Wear", state = "England", city = "", road = ""}, confidence = 7, formatted = "New York, United Kingdom", geometry = Location {lat = 55.0252998, lng = -1.4869496}},Result {bounds = Bounds {northeast = Location {lat = 30.8585202, lng = -87.1808048}, southwest = Location {lat = 30.8185202, lng = -87.2208048}}, components = Components {country = "United States of America", country_code = "US", county = "Santa Rosa County", state = "Florida", city = "", road = ""}, confidence = 7, formatted = "New York, Santa Rosa County, Florida, United States of America", geometry = Location {lat = 30.8385202, lng = -87.2008048}},Result {bounds = Bounds {northeast = Location {lat = 39.7052874, lng = -93.9068836}, southwest = Location {lat = 39.6652874, lng = -93.9468836}}, components = Components {country = "United States of America", country_code = "US", county = "Caldwell County", state = "Missouri", city = "", road = ""}, confidence = 7, formatted = "New York, Caldwell County, Missouri, United States of America", geometry = Location {lat = 39.6852874, lng = -93.9268836}},Result {bounds = Bounds {northeast = Location {lat = 40.8716701, lng = -93.2399318}, southwest = Location {lat = 40.8316701, lng = -93.2799318}}, components = Components {country = "United States of America", country_code = "US", county = "Wayne County", state = "Iowa", city = "", road = ""}, confidence = 7, formatted = "New York, Wayne County, Iowa, United States of America", geometry = Location {lat = 40.8516701, lng = -93.2599318}},Result {bounds = Bounds {northeast = Location {lat = 37.0089428, lng = -88.9325629}, southwest = Location {lat = 36.9689428, lng = -88.9725629}}, components = Components {country = "United States of America", country_code = "US", county = "Ballard County", state = "Kentucky", city = "", road = ""}, confidence = 7, formatted = "New York, Ballard County, Kentucky, United States of America", geometry = Location {lat = 36.9889428, lng = -88.9525629}},Result {bounds = Bounds {northeast = Location {lat = 35.0786534, lng = -107.5072717}, southwest = Location {lat = 35.0386534, lng = -107.5472717}}, components = Components {country = "United States of America", country_code = "US", county = "Cibola County", state = "New Mexico", city = "", road = ""}, confidence = 7, formatted = "New York, Cibola County, New Mexico, United States of America", geometry = Location {lat = 35.0586534, lng = -107.5272717}},Result {bounds = Bounds {northeast = Location {lat = 32.1879321, lng = -95.6491277}, southwest = Location {lat = 32.1479321, lng = -95.6891277}}, components = Components {country = "United States of America", country_code = "US", county = "Henderson County", state = "Texas", city = "", road = ""}, confidence = 7, formatted = "New York, Henderson County, Texas, United States of America", geometry = Location {lat = 32.1679321, lng = -95.6691277}}], status = Status {code = 200, message = "OK"}, thanks = "For using an OpenCage Data API", timestamp = Timestamp {created_http = "Tue, 26 Aug 2014 09:02:32 GMT", created_unix = 1409043752}, total_results = 10, we_are_hiring = "http://lokku.com/#jobs"})
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
-- [Result {bounds = Bounds {northeast = Location {lat = 48.3683537, lng = 16.5325042}, southwest = Location {lat = 48.0483537, lng = 16.2125042}}, components = Components {country = "Austria", country_code = "AT", county = "", state = "Vienna", city = "Vienna", road = ""}, confidence = 3, formatted = "Vienna, Vienna, Austria", geometry = Location {lat = 48.2083537, lng = 16.3725042}},Result {bounds = Bounds {northeast = Location {lat = 48.3226679, lng = 16.5775132}, southwest = Location {lat = 48.1179069, lng = 16.1819044}}, components = Components {country = "Austria", country_code = "AT", county = "", state = "Vienna", city = "", road = ""}, confidence = 4, formatted = "Vienna, Austria", geometry = Location {lat = 48.2202097, lng = 16.3712159737999}},Result {bounds = Bounds {northeast = Location {lat = 47.1759223, lng = 1.2131909}, southwest = Location {lat = 46.0482926, lng = -0.1049762}}, components = Components {country = "France", country_code = "FR", county = "Vienne", state = "Poitou-Charentes", city = "", road = ""}, confidence = 1, formatted = "Vienne, Poitou-Charentes, France", geometry = Location {lat = 46.61211655, lng = 0.465407009663971}},Result {bounds = Bounds {northeast = Location {lat = 38.921824, lng = -77.2412649}, southwest = Location {lat = 38.878494, lng = -77.2847629}}, components = Components {country = "United States of America", country_code = "US", county = "Fairfax County", state = "Virginia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Fairfax County, Virginia, United States of America", geometry = Location {lat = 38.9013013, lng = -77.2651585}},Result {bounds = Bounds {northeast = Location {lat = 48.8339836, lng = 21.9886232}, southwest = Location {lat = 48.7939836, lng = 21.9486232}}, components = Components {country = "Slovakia", country_code = "SK", county = "", state = "Region of Ko\353ice", city = "", road = ""}, confidence = 7, formatted = "Vinn\233, Vinn\233, 072 31, Region of Ko\353ice, Slovakia", geometry = Location {lat = 48.8139836, lng = 21.9686232}},Result {bounds = Bounds {northeast = Location {lat = 38.197853, lng = -91.9348889}, southwest = Location {lat = 38.175516, lng = -91.9629559}}, components = Components {country = "United States of America", country_code = "US", county = "Maries County", state = "Missouri", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Maries County, Missouri, United States of America", geometry = Location {lat = 38.1867085, lng = -91.947112}},Result {bounds = Bounds {northeast = Location {lat = 39.348538, lng = -81.5073039}, southwest = Location {lat = 39.296021, lng = -81.5564409}}, components = Components {country = "United States of America", country_code = "US", county = "Wood County", state = "West Virginia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Wood County, West Virginia, United States of America", geometry = Location {lat = 39.3270191, lng = -81.5484578}},Result {bounds = Bounds {northeast = Location {lat = 32.112459, lng = -83.7615469}, southwest = Location {lat = 32.066248, lng = -83.8138599}}, components = Components {country = "United States of America", country_code = "US", county = "Dooly County", state = "Georgia", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Dooly County, Georgia, United States of America", geometry = Location {lat = 32.0915577, lng = -83.7954518}},Result {bounds = Bounds {northeast = Location {lat = 37.432122, lng = -88.8680469}, southwest = Location {lat = 37.401787, lng = -88.9115599}}, components = Components {country = "United States of America", country_code = "US", county = "Johnson County", state = "Illinois", city = "Vienna", road = ""}, confidence = 7, formatted = "Vienna, Johnson County, Illinois, United States of America", geometry = Location {lat = 37.4153295, lng = -88.8978435}},Result {bounds = Bounds {northeast = Location {lat = 38.489516, lng = -75.8216619}, southwest = Location {lat = 38.4806818, lng = -75.8319469}}, components = Components {country = "United States of America", country_code = "US", county = "Dorchester County", state = "Maryland", city = "Vienna", road = ""}, confidence = 9, formatted = "Vienna, Dorchester County, Maryland, United States of America", geometry = Location {lat = 38.4848393, lng = -75.8246563}}]
geocodeVienna :: IO [Geocoder.Result]
geocodeVienna = Geocoder.geocode (T.pack "Vienna") myDeveloperkey

-- | geocodes Vienna where the result should be converted into german
--
-- >>> geocodeAustriaGermanResult
-- Österreich
-- Dubai, Dubai, Austria, Vereinigte Arabische Emirate
-- Austria, Recoleta, Barrio Norte, Recoleta, Ciudad Autónoma de Buenos Aires, Ciudad Autónoma de Buenos Aires, Argentinien
-- Austria, Población Iansa, Chillán, Provincia de Ñuble, VIII Región del Biobío, Chile
-- Austria, Coquimbo, Provincia de Elqui, IV Región de Coquimbo, Chile
-- Austria, Peníscola / Peñíscola, Baix Maestrat, Valencianische Gemeinschaft, Spanien
-- Austria, Mota del Cuervo, Provincia de Cuenca, Kastilien-La Mancha, Spanien
-- Austria, Carlos Paz, Departamento Punilla, Córdoba, Argentinien
-- Austria, Juan Vucetich, José C. Paz, Partido de José C. Paz, Buenos Aires, Argentinien
-- Austria, Lagomarsino, Partido del Pilar, Buenos Aires, Argentinien
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



