{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.Foldable

import Sit.Abs
import Sit.Lex
import Sit.Par
import Sit.Print

import TypeChecker

type Err = Either String

main :: IO ()
main = getArgs >>= \case
  [file] -> check =<< readFile file
  _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage: Sit <SourceFile>"
  exitFailure

-- | Handle error by failing hard.

failOnErr :: String -> Err a -> IO a
failOnErr msg = \case
  Right a  -> return a
  Left err -> exitMsg $ unlines [ msg , err ]

exitMsg :: String -> IO a
exitMsg msg = do
  putStrLn msg
  exitFailure

-- | Run the type checker on file contents
check :: String -> IO ()
check txt = do
  Prg decls <- failOnErr "PARSE ERROR" $ pPrg $ myLexer txt
  -- putStrLn "Parsed the following declarations"
  -- forM_ decls $ \ d -> do
  --   putStrLn $ printTree d
  either (\ err -> exitMsg $ unlines [ "TYPE ERROR" , err ]) return $ typeCheck decls
