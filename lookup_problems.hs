module Main where

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       getResponseBody a

data Problem = Problem {
  problemid :: String,
  problemsize :: Int,
  operators :: OperatorSet,
  solved :: Bool
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
                                                              operators = toOperatorSet ops,
                                                              solved = slved }
                             where Ok myid = readJSON idjson
                                   Just idjson = lookup "id" (fromJSObject o)
                                   Ok mysize = readJSON szjson
                                   Just szjson = lookup "size" (fromJSObject o)
                                   ops = case readJSON opsjson of
                                     Ok x -> x
                                     Error e -> error $ e ++ " with " ++ show opsjson
                                   Just opsjson = lookup "operators" (fromJSObject o)
                                   slved =
                                     case lookup "solve" (fromJSObject o) of
                                       Nothing -> False
                                       Just x ->
                                         case readJSON x of
                                           Ok x' -> x'
                                           Error e -> error $ e ++ " solved with " ++ show x
                       return $ map toProblem a

saveProblem :: Problem -> IO ()
saveProblem p =
  do let dir = "problems/" ++ show (problemsize p) ++ "/" ++ problemid p
     createDirectoryIfMissing True dir
     let opsname = dir ++ "/operators"
     writeFile (dir ++ "/operators") $ show (toStrings $ operators p) ++ "\n"
     if solved p
       then writeFile (dir ++ "/solved") $ show (solved p) ++ "\n"
       else do exists <- doesFileExist (dir ++ "/solved")
               if exists then removeFile (dir ++ "/solved") else return ()

main = do probs <- getProblems
          putStrLn $ unlines $ map show probs
          mapM_ saveProblem probs
          print $ length probs

    -- id: string;
    -- size: number;
    -- operators: string[];
    -- solved?: boolean;
    -- timeLeft?: number
