module Main where

import Data.Word ( Word64 )
import qualified Data.Map as Map

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )
import System.Environment ( getArgs )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do putStrLn ("body is:\n" ++ body)
                       a <- simpleHTTP (postRequestWithBody (url path) "text/text" body)
                       getResponseBody a

data Problem = Problem {
  problemid :: String,
  problemsize :: Int,
  operators :: OperatorSet,
  solved :: Bool
  }
             deriving ( Show )

submitEval :: String -> [Word64] -> IO [Word64]
submitEval ident g =
  do d <- getdata "eval" ("{\"id\":" ++ show ident ++ ",\"arguments\":" ++ hexes g ++ "}")
     putStrLn d
     case decode d of
       Error e -> fail $ "Error: " ++ e
       Ok a -> do let pars :: JSValue -> [Word64]
                      pars (JSObject o) = map read strings
                        where Just outs = lookup "outputs" (fromJSObject o)
                              Ok strings = readJSON outs
                  return $ pars a

readTrain :: Int -> String -> IO Problem
readTrain sz ident =
  do let dir = "trainings/" ++ show sz ++ "/" ++ ident
     let opsname = dir ++ "/operators"
     ops <- readFile opsname
     alreadydone <- doesFileExist (dir ++ "/solved")
     return Problem {
       problemid = ident,
       problemsize = sz,
       operators = toOperatorSet $ read ops,
       solved = alreadydone
       }

main = do args <- getArgs
          let [nstr,i] = if length args == 2 then args else ["5", "VOG68zQWPy4L1Vu8hginHq02"]
          let n = read nstr
          tr <- readTrain n i
          putStrLn $ show tr
          let (g, m) = solver (problemsize tr) (operators tr)
          a <- submitEval i g
          print a
          putStrLn $ hexes a
          putStrLn $ show m
          case Map.lookup a m of
            Nothing -> fail "This is impossible!"
            Just [] -> fail "coudn't happen"
            Just [p] -> putStrLn $ lispify p
            Just ps -> putStrLn $ "Could be one of " ++ show (length ps)
