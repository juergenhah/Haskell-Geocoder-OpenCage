Haskell-Geocoder-OpenCage
=========================

The request and response are converted into Haskell data types.


So far geocoding is implemented, if requested I can implement reverse geocoding too.

Dependencies:
------------
It uses the [wreq](http://www.serpentine.com/wreq/) package to wrap HTTP.


Code Examples:
------------

shortest way to geocode
```haskell  
import qualified GeoCage as GeoCage
import qualified Data.Text as T (pack,unpack)

myDeveloperkey = T.pack "your-dev-key"
geocodeNewYork = GeoCage.getAPIResponse (T.pack "New York") myDeveloperkey
```

will result in:
```
Just (ResultBody 
   { licenses = [License {name = "CC-BY-SA", url = "http://creativecommons.org/licenses/by-sa/3.0/"}
               ,License {name = "ODbL", url = "http://opendatacommons.org/licenses/odbl/summary/"}]
   , rate = Rate {limit = 2500, remaining = 2397, reset = 1.4086656e9}
   , results = [Result { bounds = Bounds { northeast = Location {lat = 40.917577, lng = -73.7001714}
                                        , southwest = Location {lat = 40.477399, lng = -74.2590899}}
                       , components = Components {country = "United States of America"
                                                 , country_code = "US"
                                                 , county = ""
                                                 , state = "New York"
                                                 , city = "New York City"
                                                 , road = ""}
                       , confidence = 2
                       , formatted = "New York City, New York City, New York, United States of America"
                       , geometry = Location {lat = 40.7305991, lng = -73.9865812}}
                ,Result {bounds = Bounds {northeast = Location {lat = 45.0158611, lng = -71.8527116}, southwest = Location {lat = 40.477399, lng = -79.76251}}, components = Components {country = "United States of America", country_code = "US", county = "", state = "New York", city = "", road = ""}, confidence = 1, formatted = "New York, United States of America", geometry = Location {lat = 43.1561681, lng = -75.8449946}}
                ,Result {bounds = Bounds {northeast = Location {lat = 41.4047714, lng = -73.340764}, southwest = Location {lat = 40.1247714, lng = -74.620764}}, components = Components {country = "United States of America", country_code = "US", county = "New York", state = "New York", city = "", road = ""}, confidence = 1, formatted = "New York, New York City, New York, United States of America", geometry = Location {lat = 40.7647714, lng = -73.980764}}
                ,Result {bounds = Bounds {northeast = Location {lat = 55.0452998, lng = -1.4669496}, southwest = Location {lat = 55.0052998, lng = -1.5069496}}, components = Components {country = "United Kingdom", country_code = "gb", county = "Tyne and Wear", state = "England", city = "", road = ""}, confidence = 7, formatted = "New York, United Kingdom", geometry = Location {lat = 55.0252998, lng = -1.4869496}}
                ,Result {bounds = Bounds {northeast = Location {lat = 30.8585202, lng = -87.1808048}, southwest = Location {lat = 30.8185202, lng = -87.2208048}}, components = Components {country = "United States of America", country_code = "US", county = "Santa Rosa County", state = "Florida", city = "", road = ""}, confidence = 7, formatted = "New York, Santa Rosa County, Florida, United States of America", geometry = Location {lat = 30.8385202, lng = -87.2008048}}
                ,Result {bounds = Bounds {northeast = Location {lat = 39.7052874, lng = -93.9068836}, southwest = Location {lat = 39.6652874, lng = -93.9468836}}, components = Components {country = "United States of America", country_code = "US", county = "Caldwell County", state = "Missouri", city = "", road = ""}, confidence = 7, formatted = "New York, Caldwell County, Missouri, United States of America", geometry = Location {lat = 39.6852874, lng = -93.9268836}}
                ,Result {bounds = Bounds {northeast = Location {lat = 40.8716701, lng = -93.2399318}, southwest = Location {lat = 40.8316701, lng = -93.2799318}}, components = Components {country = "United States of America", country_code = "US", county = "Wayne County", state = "Iowa", city = "", road = ""}, confidence = 7, formatted = "New York, Wayne County, Iowa, United States of America", geometry = Location {lat = 40.8516701, lng = -93.2599318}}
                ,Result {bounds = Bounds {northeast = Location {lat = 37.0089428, lng = -88.9325629}, southwest = Location {lat = 36.9689428, lng = -88.9725629}}, components = Components {country = "United States of America", country_code = "US", county = "Ballard County", state = "Kentucky", city = "", road = ""}, confidence = 7, formatted = "New York, Ballard County, Kentucky, United States of America", geometry = Location {lat = 36.9889428, lng = -88.9525629}}
                ,Result {bounds = Bounds {northeast = Location {lat = 35.0786534, lng = -107.5072717}, southwest = Location {lat = 35.0386534, lng = -107.5472717}}, components = Components {country = "United States of America", country_code = "US", county = "Cibola County", state = "New Mexico", city = "", road = ""}, confidence = 7, formatted = "New York, Cibola County, New Mexico, United States of America", geometry = Location {lat = 35.0586534, lng = -107.5272717}}
                ,Result {bounds = Bounds {northeast = Location {lat = 32.1879321, lng = -95.6491277}, southwest = Location {lat = 32.1479321, lng = -95.6891277}}, components = Components {country = "United States of America", country_code = "US", county = "Henderson County", state = "Texas", city = "", road = ""}, confidence = 7, formatted = "New York, Henderson County, Texas, United States of America", geometry = Location {lat = 32.1679321, lng = -95.6691277}}]
    , status = Status {code = 200, message = "OK"}
    , thanks = "For using an OpenCage Data API"
    , timestamp = Timestamp {created_http = "Thu, 21 Aug 2014 15:33:38 GMT", created_unix = 1408635218}
    , total_results = 10
    , we_are_hiring = "http://lokku.com/#jobs"})


```
There are more functions where further parameters can be included, see the examples.
