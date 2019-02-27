{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString as BS
import Geocoder
import Test.HUnit
import Test.Hspec
import Text.RawString.QQ

main :: IO ()
main =
  hspec $ do
    describe "Deserialization" $ do
      it "handle reverse geocoding responses correctly" $ do
        case eitherDecodeStrict validReverseGeocodingResponse :: Either String ResponseBody of
          Right response ->
            (components . head . results $ response) `shouldBe`
            brandenburgGateComponents
          Left msg -> assertFailure msg
      it "handle errors correctly" $ do
        case eitherDecodeStrict validError :: Either String ResponseBody of
          Right response -> (code . status $ response) `shouldBe` 403
          Left msg -> assertFailure msg

validError :: BS.ByteString
validError =
  [r|
      {
          "documentation" : "https://geocoder.opencagedata.com/api",
          "licenses" : [
             {
                "name" : "CC-BY-SA",
                "url" : "http://creativecommons.org/licenses/by-sa/3.0/"
             }
          ],
          "rate": {
            "limit": 15000,
            "remaining": 14943,
            "reset": 1551139200
          },
          "results" : [],
          "status" : {
             "code" : 403,
             "message" : "Denied!"
          },
          "stay_informed" : {
             "blog" : "https://blog.opencagedata.com",
             "twitter" : "https://twitter.com/opencagedata"
          },
          "thanks" : "For using an OpenCage Data API",
          "timestamp" : {
             "created_http": "Mon, 25 Feb 2019 08:33:17 GMT",
             "created_unix": 1551083597
          },
          "total_results" : 0
      }
 |]

brandenburgGateComponents :: Components
brandenburgGateComponents =
  Components
    { iso_3166_1_alpha_2 = "DE"
    , iso_3166_1_alpha_3 = ""
    , componentType = "attraction"
    , county = ""
    , city = "Berlin"
    , city_district = "Mitte"
    , country = "Germany"
    , country_code = "de"
    , house_number = "1"
    , political_union = "European Union"
    , postcode = "10117"
    , road = "Pariser Platz"
    , state = "Berlin"
    , suburb = "Mitte"
    }

validReverseGeocodingResponse :: BS.ByteString
validReverseGeocodingResponse =
  [r|
       {
          "documentation" : "https://geocoder.opencagedata.com/api",
          "licenses" : [
             {
                "name" : "CC-BY-SA",
                "url" : "http://creativecommons.org/licenses/by-sa/3.0/"
             }
          ],
          "rate" : {
             "limit" : 2500,
             "remaining" : 2499,
             "reset" : 2500
          },
          "results" : [
             {
                "bounds" : {
                   "northeast" : {
                      "lat" : 52.5164327,
                      "lng" : 13.377825
                   },
                   "southwest" : {
                      "lat" : 52.5161167,
                      "lng" : 13.37758
                   }
                },
                "components" : {
                   "ISO_3166-1_alpha-2" : "DE",
                   "_type" : "attraction",
                   "attraction" : "Brandenburg Gate",
                   "city" : "Berlin",
                   "city_district" : "Mitte",
                   "country" : "Germany",
                   "country_code" : "de",
                   "house_number" : "1",
                   "political_union" : "European Union",
                   "postcode" : "10117",
                   "road" : "Pariser Platz",
                   "state" : "Berlin",
                   "suburb" : "Mitte"
                },
                "confidence" : 9,
                "formatted" : "Brandenburg Gate, Pariser Platz 1, 10117 Berlin, Germany",
                "geometry" : {
                   "lat" : 52.5162767,
                   "lng" : 13.3777025
                }
             }
          ],
          "status" : {
             "code" : 200,
             "message" : "OK"
          },
          "stay_informed" : {
             "blog" : "https://blog.opencagedata.com",
             "twitter" : "https://twitter.com/opencagedata"
          },
          "thanks" : "For using an OpenCage Data API",
          "timestamp" : {
             "created_http": "Mon, 25 Feb 2019 08:33:17 GMT",
             "created_unix": 1551083597
          },
          "total_results" : 1
       }
    |]
