module Day07
    ( day07a
    , day07b
    )
where

import           Data.List
import           Control.Monad
import           System.FilePath
import           Utils
import           Intcode
import           Data.Maybe

type PhaseSequence = [Int]

day07a :: IO ()
day07a = readAndProcessProg ("inputs" </> "day07_input.txt") run
  where
    run prog = mapM (runPipe prog) perms >>= print . maximum
    perms = permutations [0 .. 4]
    runPipe :: Program -> PhaseSequence -> IO Int
    runPipe prog = foldM exec 0
      where
        exec input phase = do
            (_, [output]) <- runProg prog [] [phase, input] 0
            return output

day07b :: IO ()
day07b = readAndProcessProg ("inputs" </> "day07_input.txt") run
  where
    run prog = mapM (runPipe prog) perms >>= print . maximum
    seqNums = [5 .. 9]
    perms   = permutations seqNums
    runPipe :: Program -> PhaseSequence -> IO Int
    runPipe prog phases = do
        amps                  <- replicateM (length seqNums) (initVec prog)
        (firstResult, revips) <- foldM execInit (0, []) (zip phases amps)
        let ips = reverse revips
        feedback (zip amps ips) firstResult
      where
        execInit (input, ips) (phase, amp) = do
            ([output], Just ip) <- runProgVec amp 0 True [] [phase, input]
            return (output, ip : ips)

feedback :: [(ProgVec, IP)] -> Int -> IO Int
feedback progips initVal = do
    (result, revips) <- foldM execInit (Just initVal, []) progips
    let ips = reverse revips
    case result of
        Nothing  -> return initVal
        Just val -> feedback (zip (map fst progips) (catMaybes ips)) val
  where
    execInit (input, ips) (prog, currIP) = case input of
        Just input' -> do
            (output, newIP) <- runProgVec prog currIP True [] [input']
            let val = if null output then Nothing else Just $ head output
            return (val, newIP : ips)
        Nothing -> return (Nothing, [])
