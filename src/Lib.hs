module Lib
    ( someFunc
    ) where

import System.IO
--import Control.Monad.State
import Data.Map(Map)
import Data.Map as Map

data Cleaness = Clean | Dirty
data Operation = Run Cleaness | Exit | Save
type CurState = (Operation, Cleaness)
--type AppState = State CurState (Map String Int)
type AppData = Map String Int
type AppState = (CurState, AppData)

-- Valid state transitions:
-- * Run Clean -> Run Dirty
-- * Run Clean -> Quit
-- * Run Dirty -> Save
-- * Save -> Run Clean

-- TODO
-- * run / save: should be wrapped in IO monad
-- * move readChar to run
-- * Add nextOp Selector to transition
transition :: CurState -> CurState -> (Bool, Maybe String)
transition old new = case (old, new) of
  (Run _, Run _)         -> (True,  Nothing)
  (Run Clean, Run Dirty) -> (True,  Nothing)
  (Run Clean, Quit)      -> (True,  Just "Quitting")
  (Run Dirty, Quit)      -> (False, Just "Please save before exiting")
  (Run Dirty, Save)      -> (True,  Just "saving")
  (Save, Run Clean)      -> (True,  Nothing)
  (_, _)                 -> (False, Nothing)

run :: AppState -> Char -> AppState
run cur@(state, appData) c = case c of
  'q' -> (exitState state, appData)
  's' -> (Save, appData)
  'd' -> (Run Dirty, appData)
  _ -> cur

loop :: Handle -> AppState -> IO ()
loop handle state =
  let runner = run state in
  do
    c <- hGetChar handle
    case status of
      --(Run, _)  -> loop handle nState
      --(Save, Dirty) -> putStrLn "SAVING" >> loop handle ((Run, Clean), d)
      --(Save, Clean) -> loop handle ((Run, Clean), d)
      --(Exit, Dirty) -> (putStrLn "please save before exiting (s)") >> loop handle ((Run, Dirty), d)
      --(Exit, Clean) -> putStrLn "QUITTING"


setup :: Handle -> IO ()
setup handle = hSetBuffering handle NoBuffering >> hSetEcho stdin False

teardown :: Handle -> IO ()
teardown handle = hSetEcho handle True

baseFunc :: Handle -> IO ()
baseFunc handle =
  let initState = (Run Clean, Map.empty) in
  (setup handle) >> (loop handle initState) >> (teardown handle)

someFunc :: IO ()
someFunc = putStrLn "RUNNING:" >> baseFunc stdin
