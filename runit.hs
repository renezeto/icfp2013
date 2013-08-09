module Main where

import Ast
import Network.HTTP
import Text.JSON

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do a <- simpleHTTP (postRequestWithBody (url "myproblems") "text/text" "")
                       getResponseBody a

data Problem = Problem {
  problemid :: String,
  problemsize :: Int,
  operators :: OperatorSet
  }
             deriving ( Show )

getProblems :: IO [Problem]
getProblems =
  do b <- getdata "myproblems" ""
     case decode b of
            Error e -> fail $ "Error: " ++ e
            Ok a -> do let toProblem :: JSValue -> Problem
                           toProblem (JSObject o) = Problem { problemid = myid,
                                                              problemsize = mysize,
                                                              operators = toOperatorSet ops }
                             where Ok myid = readJSON idjson
                                   Just idjson = lookup "id" (fromJSObject o)
                                   Ok mysize = readJSON szjson
                                   Just szjson = lookup "size" (fromJSObject o)
                                   ops = case readJSON opsjson of
                                     Ok x -> x
                                     Error e -> error $ e ++ " with " ++ show opsjson
                                   Just opsjson = lookup "operators" (fromJSObject o)
                       return $ map toProblem a


main = do probs <- getProblems
          putStrLn $ unlines $ map show probs
          print $ length probs

    -- id: string;
    -- size: number;
    -- operators: string[];
    -- solved?: boolean;
    -- timeLeft?: number
