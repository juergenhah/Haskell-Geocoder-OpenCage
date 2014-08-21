
import qualified GeoCage as GeoCage

import qualified Data.Text as T (pack,unpack)

myDeveloperkey = T.pack "your-dev-key"

geocodeNewYork = GeoCage.getAPIResponse (T.pack "New York") myDeveloperkey

checkLicensesforDelhi = do
  let params = GeoCage.defaultParams {GeoCage._para_q = T.pack "Delhi",GeoCage._para_key=myDeveloperkey}
  eitherResponse<-GeoCage.getAPIResponseWith params
  case eitherResponse of
    Nothing -> putStrLn "no result delivered from the geocoder"
    Just r -> do 
      putStrLn "which licenses are used:"
      putStrLn $ show (GeoCage.licenses r)
  
howManyRequestsLeft :: IO Integer
howManyRequestsLeft = do
  let params = GeoCage.defaultParams {GeoCage._para_q = T.pack "Delhi",GeoCage._para_key=myDeveloperkey}
  eitherResponse<-GeoCage.getAPIResponseWith params
  case eitherResponse of
    Nothing -> return (-1)
    Just r -> do 
      return $ (GeoCage.remaining.GeoCage.rate $ r)

geocodeVienna = GeoCage.geocode (T.pack "Vienna") myDeveloperkey


geocodeAustriaGermanResult= do
  let params= GeoCage.defaultParams {GeoCage._para_q = T.pack "Austria"
                                    ,GeoCage._para_key=myDeveloperkey
                                    ,GeoCage._para_language= Just $  T.pack "de"}
  results <- GeoCage.geocodeWith params
  print $ map (T.unpack.GeoCage.formatted) results


testGeocodeAPI1 = do 
  let tokens = map T.pack ["Berlin","München","Tokyo","invalid","San Francisco","White House"]
  GeoCage.geocodeGeoTokensList tokens myDeveloperkey



