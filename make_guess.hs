module Main where

import Data.Word ( Word64 )
import qualified Data.Map as Map
import System.Exit ( exitSuccess )
import System.CPUTime ( getCPUTime )

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

submitGuess :: String -> Ast -> IO (Maybe (Word64, Word64, Word64))
submitGuess ident p =
  do d <- getdata "guess" ("{\"id\":" ++ show ident ++ ",\"program\":" ++ show (lispify p) ++ "}")
     putStrLn d
     case decode d of
       Error e -> fail $ "submitGuess error " ++ e
       Ok (JSObject a) ->
         case lookup "status" (fromJSObject a) of
           Nothing -> fail "shoudlnt' be possible"
           Just statstr ->
             case readJSON statstr of
               Ok "win" -> return Nothing
               Ok "error" -> fail "error says the status"
               Ok "mismatch" ->
                 case lookup "values" (fromJSObject a) of
                   Nothing -> fail "cannot happend"
                   Just valstr ->
                     case readJSON valstr of
                       Ok (a,b,c) -> return (Just (read a, read b, read c))

makeGuess :: String -> [Ast] -> IO ()
makeGuess _ [] = fail "I have no idea!"
makeGuess ident [a] = do submitGuess ident a
                         fail "oops"
makeGuess ident (b:bs) =
  do r <- submitGuess ident b
     case r of
       Nothing -> putStrLn "We won, we won!!!"
       Just (inp, out, _) ->
         do putStrLn $ "I could do better on " ++ niceHex inp
            makeGuess ident (filter (\p -> eval p inp 0 0 == out) bs)

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

timeMe :: String -> Integer -> IO Integer
timeMe job start =
  do stop <- getCPUTime
     putStrLn $ job ++ " took " ++ show (fromIntegral (stop-start)/1.0e12 :: Double) ++ " seconds"
     getCPUTime

main = do args0 <- getArgs
          let (timeonly, args) = if length args0 == 3 && head args0 == "time"
                                 then (True, tail args0)
                                 else (False, args0)
              [nstr,i] = if length args == 2 then args else ["5", "VOG68zQWPy4L1Vu8hginHq02"]
              n = read nstr
          tr <- readTrain n i
          putStrLn $ show tr
          start <- timeMe "File IO" 0
          putStrLn $ "I count " ++ show (length $ enumerate_program (problemsize tr) (operators tr)) 
            ++ " programs"
          start <- timeMe "Generating programs" start
          putStrLn $ "I count " ++ show (length guesses) ++ " guesses"
          start <- timeMe "Generating guesses" start
          let (g, m) = solver (problemsize tr) (operators tr)
          putStrLn $ "minsize is " ++ show (minimum_size (operators tr))
          putStrLn $ "problem size is " ++ show (problemsize tr)
          putStrLn $ "number programs is " ++ show (length $ concat $ Map.elems m)
          putStrLn $ "number distinguished outputs is " ++ show (length $ Map.elems m)
          timeMe "solver" start
          if timeonly then exitSuccess
                      else return ()
          a <- submitEval i g
          print a
          putStrLn $ hexes a
          putStrLn $ show m
          case Map.lookup a m of
            Nothing -> do let ps = concat $ Map.elems m
                          putStrLn $ unlines $ map lispify ps
                          fail "This is impossible!"
            Just [] -> fail "coudn't happen"
            Just ps -> do putStrLn $ "Could be one of " ++ show (length ps)
                          putStrLn $ unlines $ map lispify ps
                          makeGuess i ps
