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
import           Data.Int

type PhaseSequence = [Int64]

day07a :: IO ()
day07a = readAndProcessProg ("inputs" </> "day07_input.txt") run
  where
    run prog = mapM (runPipe prog) perms >>= print . maximum
    perms = permutations [0 .. 4]
    runPipe :: Program -> PhaseSequence -> IO MemType
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
    runPipe :: Program -> PhaseSequence -> IO MemType
    runPipe prog phases = do
        amps                  <- replicateM (length seqNums) (initVec prog)
        (firstResult, revips) <- foldM execInit (0, []) (zip phases amps)
        let ips = reverse revips
        feedback (zip amps ips) firstResult
      where
        execInit :: (MemType, [IP]) -> (MemType, ProgVec) -> IO (MemType, [IP])
        execInit (input, ips) (phase, amp) = do
            Just state <- execute amp (mkInitState {inputs = [phase, input]})
            return (output state, ip state : ips)

feedback :: [(ProgVec, IP)] -> MemType -> IO MemType
feedback progips initVal = do
    (result, revips) <- foldM execInit (Just initVal, []) progips
    let ips = reverse revips
    case result of
        Nothing  -> return initVal
        Just val -> feedback (zip (map fst progips) ips) val
  where
    execInit (input, ips) (prog, currIP) = case input of
        Just input' -> do
            maybeState <- execute prog (mkInitState {ip = currIP, inputs = [input']})
            case maybeState of
              Nothing -> return (Nothing, [])
              Just state -> return (Just $ output state, ip state : ips)
        Nothing -> return (Nothing, [])
