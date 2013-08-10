module Main where

import Data.Word ( Word64 )
import System.Exit ( exitSuccess )
import System.CPUTime ( getCPUTime )
import System.Random

import Ast
import Network.HTTP
import Text.JSON
import System.Directory ( createDirectoryIfMissing, removeFile, doesFileExist )
import System.Environment ( getArgs )

url path = "http://icfpc2013.cloudapp.net/" ++ path ++ "?auth=0175jv6XdpWdKm9pYxVcBgmSMCIlP4aVxQxZ3PqOvpsH1H"

getdata path body = do if length body < 128
                         then putStrLn ("body is:\n" ++ body)
                         else putStrLn ("body length is: " ++ show (length body))
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
     if length d < 128
       then putStrLn $ "Response was: " ++ d
       else putStrLn $ "Response was length " ++ show (length d)
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
makeGuess ident (b:bs) =
  do r <- submitGuess ident b
     case r of
       Nothing -> putStrLn "We won, we won!!!"
       Just (inp, out, _) ->
         do putStrLn $ "I could do better on " ++ niceHex inp
            makeGuess ident (filter (\p -> eval p inp == out) bs)

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

printNumber :: Int -> IO ()
printNumber nprograms =
  if nprograms > 1024*1024*1024
  then putStrLn $ "I count " ++
       show (round $ fromIntegral nprograms/1024.0/1024.0/1024.0) ++ " gigaprograms"
  else if nprograms > 1024*1024
       then putStrLn $ "I count " ++
            show (round $ fromIntegral nprograms/1024.0/1024.0) ++ " megaprograms"
       else if nprograms > 1024
            then putStrLn $ "I count " ++
                 show (round $ fromIntegral nprograms/1024.0) ++ " kiloprograms"
            else putStrLn $ "I count " ++
                 show (round $ fromIntegral nprograms) ++ " programs"

main = do args0 <- getArgs
          let (todo, args) =
                if length args0 == 3
                then case head args0 of
                       "time" -> ("time", tail args0)
                       "count-programs" -> ("count-programs", tail args0)
                else ("", args0)
              [nstr,i] = if length args == 2 then args else ["5", "VOG68zQWPy4L1Vu8hginHq02"]
              n = read nstr
          tr <- readTrain n i
          putStrLn $ show tr
          start <- timeMe "File IO" 0
          case todo of
            "time" ->
                 do let programs = enumerate_program (problemsize tr) (operators tr)
                        rndprograms = filter isrnd programs
                        isrnd p = map (eval p) guesses == rndout
                        rndout = take (length guesses) $ randoms (mkStdGen 1)
                    printNumber $ length rndprograms
                    start <- timeMe "Filtering random programs" start
                    exitSuccess
            "count-programs" ->
                 do let programs = enumerate_program (problemsize tr) (operators tr)
                    printNumber (length programs)
                    start <- timeMe "Counting programs" start
                    exitSuccess
            _ -> return ()
          a <- submitEval i guesses
          let programs = enumerate_program (problemsize tr) (operators tr)
          makeGuess i $ filter (\p -> map (eval p) guesses == a) programs
