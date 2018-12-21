module Day21
    ( day21
    , day21b
    ) where

import Lib
import Text.ParserCombinators.ReadP
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Array ((!))
import Data.Foldable
import qualified Data.List.Split as Split
import Data.Bits
import Data.Char
import Data.Either


{-
You should have been watching where you were going, because as you wander the new North Pole base, you trip and fall
into a very deep hole!

Just kidding. You're falling through time again.

If you keep up your current pace, you should have resolved all of the temporal anomalies by the next time the device
activates. Since you have very little interest in browsing history in 500-year increments for the rest of your life,
you need to find a way to get back to your present time.

After a little research, you discover two important facts about the behavior of the device:

First, you discover that the device is hard-wired to always send you back in time in 500-year increments. Changing
this is probably not feasible.

Second, you discover the activation system (your puzzle input) for the time travel module. Currently, it appears to
run forever without halting.

If you can cause the activation system to halt at a specific moment, maybe you can make the device send you so far
back in time that you cause an integer underflow in time itself and wrap around back to your current time!

The device executes the program as specified in manual section one and manual section two.

Your goal is to figure out how the program works and cause it to halt. You can only control register 0; every other
register begins at 0 as usual.

Because time travel is a dangerous activity, the activation system begins with a few instructions which verify that
bitwise AND (via bani) does a numeric operation and not an operation as if the inputs were interpreted as strings.
If the test fails, it enters an infinite loop re-running the test instead of allowing the program to execute
normally. If the test passes, the program continues, and assumes that all other bitwise operations (banr, bori, and
borr) also interpret their inputs as numbers. (Clearly, the Elves who wrote this system were worried that someone
might introduce a bug while trying to emulate this system with a scripting language.)

What is the lowest non-negative integer value for register 0 that causes the program to halt after executing the
fewest instructions? (Executing the same instruction multiple times counts as multiple instructions executed.)
-}

type RV = Int  {- value in a register -}

data Instruction = Op { code :: (String, Before -> RV -> RV -> RV -> After)
                      , a :: RV
                      , b :: RV
                      , c :: RV
                      }
                 | Ip RV

instance Show Instruction where
  show (Ip ip) = "#ip " ++ (show ip)
  show (Op {code=(name,_), a=a, b=b, c=c}) = name ++ " " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)

type RegisterFile = Array.Array RV RV
type Before = RegisterFile
type After = RegisterFile

showRegs :: RegisterFile -> String
showRegs = show . Array.elems

parse :: [String] -> (RV, Array.Array RV Instruction, [Maybe Instruction])
parse ls =
  let raw = map (parseWith instructionParser) ls
      assembly = catMaybes $ raw
      ([Ip ip], ops) = List.partition (\i -> case i of
                                          Ip _ -> True
                                          otherwise -> False) assembly
  in (ip, Array.array (0, length ops - 1) $ zip [0..] ops, raw)

instructionParser :: ReadP Instruction
instructionParser = pragmaParser <++ opParser

pragmaParser :: ReadP Instruction
pragmaParser = do
  string "#ip "
  ip <- intParser
  eof
  return $ Ip $ fromIntegral ip

opParser :: ReadP Instruction
opParser = do
  op <- munch1 isAlpha
  let fn = opMap Map.! op
  char ' '
  a <- intParser
  char ' '
  b <- intParser
  char ' '
  c <- intParser
  eof
  return $ Op { code=(op, fn), a=fromIntegral a, b=fromIntegral b, c=fromIntegral c }



reg0 = Array.array (0,5) [(i,0) | i <- [0..5]]

store rs index val = rs Array.// [(index,val)]
fetch rs index = rs ! index

addr rs a b c = store rs c ((rs `fetch` a) + (rs `fetch` b))
addi rs a b c = store rs c ((rs `fetch` a) + b)

mulr rs a b c = store rs c ((rs `fetch` a) * (rs `fetch` b))
muli rs a b c = store rs c ((rs `fetch` a) * b)

banr rs a b c = store rs c ((rs `fetch` a) .&. (rs `fetch` b))
bani rs a b c = store rs c ((rs `fetch` a) .&. b)

borr rs a b c = store rs c ((rs `fetch` a) .|. (rs `fetch` b))
bori rs a b c = store rs c ((rs `fetch` a) .|. b)

setr rs a b c = store rs c (rs `fetch` a)
seti rs a b c = store rs c (a)

gtir rs a b c = store rs c (if a > (rs `fetch` b) then 1 else 0)
gtri rs a b c = store rs c (if (rs `fetch` a) > b then 1 else 0)
gtrr rs a b c = store rs c (if (rs `fetch` a) > (rs `fetch` b) then 1 else 0)

eqir rs a b c = store rs c (if a == (rs `fetch` b) then 1 else 0)
eqri rs a b c = store rs c (if (rs `fetch` a) == b then 1 else 0)
eqrr rs a b c = store rs c (if (rs `fetch` a) == (rs `fetch` b) then 1 else 0)

allOps = [("addr", addr), ("addi", addi), ("mulr", mulr), ("muli", muli),
          ("banr", banr), ("bani", bani), ("borr", borr), ("bori", bori),
          ("setr", setr), ("seti", seti), ("gtir", gtir), ("gtri", gtri),
          ("gtrr", gtrr), ("eqir", eqir), ("eqri", eqri), ("eqrr", eqrr)]

opMap = Map.fromList allOps



runProgram :: RV -> Array.Array RV Instruction -> RV -> RegisterFile -> IO (RV, RegisterFile)
runProgram ipBinding prog ip regs = do
  result <- runInstruction ipBinding prog ip regs
  if result == Nothing
  then return (ip, regs)
  else do
    let Just (ip', regs') = result
    runProgram ipBinding prog ip' regs'

type Trace r t = t -> RV -> RegisterFile -> IO (Either r t)
type Tracer r t = (Trace r t, t)  {- a combination of the Trace function and its state -}

runWithTrace :: RV -> Array.Array RV Instruction -> RV -> RegisterFile -> Tracer r t -> IO (Either r (RV, RegisterFile))
runWithTrace ipBinding prog ip regs (trace,t) = do
  traceOutput <- trace t ip regs
  case traceOutput of
    Left x -> return $ Left x
    Right t' -> do
      result <- runInstruction ipBinding prog ip regs
      if result == Nothing
      then return $ Right (ip, regs)
      else do
        let Just (ip', regs') = result
        runWithTrace ipBinding prog ip' regs' (trace,t')


runInstruction :: RV -> Array.Array RV Instruction -> RV -> RegisterFile -> IO (Maybe (RV, RegisterFile))
runInstruction ipBinding prog ip regs = do
  let (_,bound) = Array.bounds prog
  if ip < 0 || bound < ip
  then do
    {- putStrLn "Halts" -}
    return Nothing
  else do
    let rs = store regs ipBinding ip
        op = prog ! ip
        (_,f) = code op
        rs' = f rs (a op) (b op) (c op)
        ip' = (rs' `fetch` ipBinding) + 1
    {- putStrLn $ "ip " ++ (show ip) ++ " " ++ (showRegs rs) ++ " " ++ (show op) ++ " " ++ (showRegs rs') -}
    return $ Just (ip', rs')


day21 :: [String] -> IO ()
day21 ls = do
  let (ipb, prog, raw) = parse ls
  putStrLn $ show ipb
  putStrLn $ show raw
  result <- runWithTrace ipb prog 0 reg0 (stopAtLine 28)
  case result of
    Right (ip, rs) ->
      putStrLn $ (show ip) ++ " -> " ++ (showRegs rs)
    Left rs ->
      putStrLn $ "Trace stops with regs = " ++ (showRegs rs)

stopAtLine :: RV -> Tracer RegisterFile ()
stopAtLine n = (trace, ())
  where
    trace () ip rs = do
      if ip == n
      then return $ Left rs
      else return $ Right ()



{-

-}


day21b :: [String] -> IO ()
day21b ls = do
  let (ipb, prog, raw) = parse ls
  putStrLn $ show ipb
  putStrLn $ show raw
  result <- runWithTrace ipb prog 0 reg0 (recordRegisterAtLine 2 28)
  case result of
    Right (ip, rs) ->
      putStrLn $ (show ip) ++ " -> " ++ (showRegs rs)
    Left (repeated, previous) ->
      putStrLn $ "Trace stops with repeated register value of " ++ (show repeated) ++
                 " previous value was " ++ (show previous)

recordRegisterAtLine :: RV -> RV -> Tracer (RV, RV) (Set.Set RV, Maybe RV)
recordRegisterAtLine reg n = (trace, (Set.empty, Nothing))
  where
    trace :: Trace (RV, RV) (Set.Set RV, Maybe RV)
    trace (x, Nothing) ip rs = do
      if ip == n
      then do
        let val = rs `fetch` reg
        putStrLn $ "Initial potential value: " ++ (show val)
        return $ Right (Set.singleton val, Just val)
      else return $ Right (x, Nothing)
    trace (seen, Just prev) ip rs = do
      if ip == n
      then do
        let val = rs `fetch` reg
        if Set.member val seen
        then return $ Left (val, prev)
        else do
          putStrLn $ (show val) ++ " " ++ (show $ Set.size seen)
          return $ Right (Set.insert val seen, Just val)
      else return $ Right (seen, Just prev)
