module Day16
    ( day16
    , day16b
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


{-
As you see the Elves defend their hot chocolate successfully, you go back to falling through time. This is going to
become a problem.

If you're ever going to return to your own time, you need to understand how this device on your wrist works. You have
a little while before you reach your next destination, and with a bit of trial and error, you manage to pull up a
programming manual on the device's tiny screen.

According to the manual, the device has four registers (numbered 0 through 3) that can be manipulated by instructions
containing one of 16 opcodes. The registers start with the value 0.

Every instruction consists of four values: an opcode, two inputs (named A and B), and an output (named C), in that
order. The opcode specifies the behavior of the instruction and how the inputs are interpreted. The output, C, is
always treated as a register.

In the opcode descriptions below, if something says "value A", it means to take the number given as A literally.
(This is also called an "immediate" value.) If something says "register A", it means to use the number given as A to
read from (or write to) the register with that number. So, if the opcode addi adds register A and value B, storing
the result in register C, and the instruction addi 0 7 3 is encountered, it would add 7 to the value contained by
register 0 and store the sum in register 3, never modifying registers 0, 1, or 2 in the process.

Many opcodes are similar except for how they interpret their arguments. The opcodes fall into seven general categories:

Addition:

    addr (add register) stores into register C the result of adding register A and register B.
    addi (add immediate) stores into register C the result of adding register A and value B.

Multiplication:

    mulr (multiply register) stores into register C the result of multiplying register A and register B.
    muli (multiply immediate) stores into register C the result of multiplying register A and value B.

Bitwise AND:

    banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
    bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.

Bitwise OR:

    borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
    bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.

Assignment:

    setr (set register) copies the contents of register A into register C. (Input B is ignored.)
    seti (set immediate) stores value A into register C. (Input B is ignored.)

Greater-than testing:

    gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise,
    register C is set to 0.
    gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise,
    register C is set to 0.
    gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise,
    register C is set to 0.

Equality testing:

    eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C
    is set to 0.
    eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C
    is set to 0.
    eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C
    is set to 0.

Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number. However, you
can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work
them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which. For example, suppose
you capture the following sample:

Before: [3, 2, 1, 1]
9 2 1 2
After:  [3, 2, 2, 1]

This sample shows the effect of the instruction 9 2 1 2 on the registers. Before the instruction is executed, register
0 has value 3, register 1 has value 2, and registers 2 and 3 have value 1. After the instruction is executed, register
2's value becomes 2.

The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2, B=1, and C=2. Opcode 9 could be any of the
16 opcodes listed above, but only three of them behave in a way that would cause the result shown in the sample:

    Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1 (which has a value of 2) produces
      2, which matches the value stored in the output register, register 2.
    Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1 produces 2, which matches the value
      stored in the output register, register 2.
    Opcode 9 could be seti: value 2 matches the value stored in the output register, register 2; the number given
      for B is irrelevant.

None of the other opcodes produce the result captured in the sample. Because of this, the sample above behaves like
three opcodes.

You collect many of these samples (the first section of your puzzle input). The manual also includes a small test
program (the second section of your puzzle input) - you can ignore it for now.

Ignoring the opcode numbers, how many samples in your puzzle input behave like three or more opcodes?
-}

{-
Before: [1, 3, 1, 3]
14 0 3 0
After:  [0, 3, 1, 3]

Before: [0, 0, 2, 1]
11 0 1 1
After:  [0, 1, 2, 1]

Before: [2, 1, 2, 1]
0 0 3 3
After:  [2, 1, 2, 3]



9 2 0 0
9 3 0 2
9 1 0 3
12 3 0 2
-}


data Instruction = Op { code :: Integer
                      , a :: Integer
                      , b :: Integer
                      , c :: Integer
                      }
  deriving (Show)

type Before = [Integer]
type After = [Integer]

data Sample = Sample { before :: Before
                     , instruction :: Instruction
                     , after :: After
                     }
  deriving (Show)

opcode Sample{instruction=Op{code=opc}} = opc

parse :: [String] -> ([Sample], [Instruction])
parse ls =
  let [sampleLines, instructionLines] = Split.splitOn ["", "", ""] ls
      samples = Split.splitOn [""] sampleLines
  in (map parseSample samples, catMaybes $ map (parseWith instructionParser) instructionLines)

parseSample :: [String] -> Sample
parseSample [b,i,a] =
  let Just b' = parseWith beforeParser b
      Just i' = parseWith instructionParser i
      Just a' = parseWith afterParser a
  in  Sample { before=b', instruction=i', after=a' }

beforeParser :: ReadP Before
beforeParser = do
  string "Before: ["
  regs <- sepBy intParser (string ", ")
  string "]"
  eof
  return $ map fromIntegral regs

afterParser :: ReadP After
afterParser = do
  string "After:  ["
  regs <- sepBy intParser (string ", ")
  string "]"
  eof
  return $ map fromIntegral regs

instructionParser :: ReadP Instruction
instructionParser = do
  op <- intParser
  char ' '
  a <- intParser
  char ' '
  b <- intParser
  char ' '
  c <- intParser
  eof
  return $ Op { code=op, a=a, b=b, c=c }



store [w,x,y,z] 0 v = [v,x,y,z]
store [w,x,y,z] 1 v = [w,v,y,z]
store [w,x,y,z] 2 v = [w,x,v,z]
store [w,x,y,z] 3 v = [w,x,y,v]

fetch [w,x,y,z] 0 = w
fetch [w,x,y,z] 1 = x
fetch [w,x,y,z] 2 = y
fetch [w,x,y,z] 3 = z

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


day16 :: [String] -> IO ()
day16 ls = do
  let (samples, prog) = parse ls
      hardSamples = filter (\sample ->
        let possibles = winnow allOps sample
        in  length possibles >= 3) samples
  putStrLn $ show samples
  putStrLn $ show prog
  putStrLn $ "Number of samples: " ++ (show $ length samples)
  putStrLn $ "Length of program: " ++ (show $ length prog)
  putStrLn $ "The number of samples that map to three or more opcodes is " ++ (show $ length hardSamples)

type Operation = Before -> Integer -> Integer -> Integer -> After
type OpCode = (String, Operation)

winnow :: [OpCode] -> Sample -> [OpCode]
winnow testOps (Sample {before=rs, instruction=Op{a=a,b=b,c=c}, after=rs'}) =
  filter (\(_,op) -> op rs a b c == rs') testOps


{-
Using the samples you collected, work out the number of each opcode and execute the test program (the second section
of your puzzle input).

What value is contained in register 0 after executing the test program?
-}

day16b :: [String] -> IO ()
day16b ls = do
  let (samples, prog) = parse ls

      opcounts0 = Map.fromList [(i, 0) | i <- [0..15]]
      opcounts = foldl (\cs sample -> Map.adjust (+1) (opcode sample) cs) opcounts0 samples

      possibles = Map.fromList [(i, allOps) | i <- [0..15]]
      opcodes = foldl winnowOp possibles samples

  forM_ (Map.toList $ Map.map (map (\(n,f) -> n)) opcodes) $ do
    putStrLn . show

  let narrowed = narrow opcodes

  let result = foldl (runInstruction narrowed) [0,0,0,0] prog
  putStrLn $ show $ length samples
  putStrLn $ show $ opcounts
  putStrLn $ show $ Map.map length possibles
  putStrLn $ show $ Map.map length opcodes
  putStrLn $ show $ Map.map length narrowed
  putStrLn $ show $ result

winnowOp :: Map.Map Integer [OpCode] -> Sample -> Map.Map Integer [OpCode]
winnowOp possibles sample =
  let opc = opcode sample
      fs = possibles Map.! opc
      fs' = winnow fs sample
  in  Map.insert opc fs' possibles

{- if one opcode maps onto ORLY then no other one can -}
narrow :: Map.Map Integer [OpCode] -> Map.Map Integer [OpCode]
narrow opcodes =
  let code_name0 = opcodeMapToCodeNameSet opcodes
      code_name = narrow0 code_name0
  in  codeNameSetToOpcodeMap code_name
  where
    opcodeMapToCodeNameSet :: Map.Map Integer [OpCode] -> Set.Set (Integer, String)
    opcodeMapToCodeNameSet opcodes =
      Set.fromList $ concatMap (\(c,ops) -> [(c,n) | (n,_) <- ops]) $ Map.toList opcodes

    codeNameSetToOpcodeMap :: Set.Set (Integer, String) -> Map.Map Integer [OpCode]
    codeNameSetToOpcodeMap cns =
      let codeToNames = opcodeToNameList cns
      in  Map.map (map (\n -> (n, nameToOpCode n))) codeToNames

    nameToOpCode name = (Map.fromList allOps) Map.! name

    narrow0 :: Set.Set (Integer,String) -> Set.Set (Integer,String)
    narrow0 opcodes =
      let opc1 = narrowByCode opcodes
          opc2 = narrowByName opc1
      in  if opc2 == opcodes
          then opcodes
          else narrow0 opc2

    narrowByCode :: Set.Set (Integer,String) -> Set.Set (Integer,String)
    narrowByCode opcodes =
      let namesWithSingleOpcode = Map.filter (\opcs -> (length opcs) == 1) $ nameToOpcodeList opcodes
      in  foldl (\opcs (n,[opc]) ->
                    Set.filter (\(c',n') -> n' == n || c' /= opc) opcs)
                 opcodes $
                 Map.toList namesWithSingleOpcode

    uniqueNames :: Set.Set (Integer, String) -> Set.Set (String)
    uniqueNames opcodes = Set.map (\(c,n) -> n) opcodes

    nameToOpcodeList :: Set.Set (Integer,String) -> Map.Map String [Integer]
    nameToOpcodeList opcodes =
      foldl (\m (opc, n) -> Map.adjust (opc:) n m)
             (Map.fromList [(n,[]) | n <- Set.toList $ uniqueNames opcodes])
             (Set.toList opcodes)


    narrowByName :: Set.Set (Integer,String) -> Set.Set (Integer,String)
    narrowByName opcodes =
      let opcodesWithSingleName = Map.filter (\ns -> (length ns) == 1) $ opcodeToNameList opcodes
      in  foldl (\opcs (opc,[n]) -> Set.filter (\(c',n') -> c' == opc || n' /= n) opcs)
                 opcodes
                 (Map.toList opcodesWithSingleName)

    uniqueOpcodes :: Set.Set (Integer,String) -> Set.Set Integer
    uniqueOpcodes opcodes = Set.map (\(c,n) -> c) opcodes

    opcodeToNameList :: Set.Set (Integer,String) -> Map.Map Integer [String]
    opcodeToNameList opcodes =
      foldl (\m (opc, n) -> Map.adjust (n:) opc m)
             (Map.fromList [(c,[]) | c <- Set.toList $ uniqueOpcodes opcodes])
             (Set.toList opcodes)



runInstruction :: Map.Map Integer [OpCode] -> Before -> Instruction -> After
runInstruction opcodes rs Op{code=opc,a=a,b=b,c=c} =
  let [(_,f)] = opcodes Map.! opc
  in  f rs a b c