module Main where

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       getResponseBody a

main = do status <- getdata "status" ""
          putStrLn $ map makenice status
  where makenice ',' = '\n'
        makenice c = c
