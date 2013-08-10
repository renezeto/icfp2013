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
            Just ps -> do putStrLn $ "Could be one of " ++ show (length ps)
                          putStrLn $ unlines $ map lispify ps
                          makeGuess i ps
