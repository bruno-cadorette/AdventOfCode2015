import Data.Word
import Parser
import qualified Data.Array as A
import qualified Data.Vector as V

type Registers = A.Array Register Word32
type Program = V.Vector Instruction
data Memory = Memory { registers :: Registers, programCounter :: Int, program :: Program }

next :: Memory -> Memory
next = jump 1

jump :: Int -> Memory -> Memory
jump offset memory = memory { programCounter = programCounter memory + offset }

modifyRegister :: Register -> (Word32 -> Word32) -> Memory -> Memory
modifyRegister r f memory = memory { registers = registers memory A.// [(r, newValue)] }
    where newValue = f $ currentValue r memory

currentValue :: Register -> Memory -> Word32
currentValue r memory = registers memory A.! r
    
computeInstruction :: Instruction -> Memory -> Memory
computeInstruction (Hlf r) m = next $ modifyRegister r (`div` 2) m
computeInstruction (Tpl r) m = next $ modifyRegister r (* 3) m
computeInstruction (Inc r) m = next $ modifyRegister r (+ 1) m
computeInstruction (Jmp o) m = jump o m
computeInstruction (Jie r o) m = if even $ currentValue r m then jump o m else next m
computeInstruction (Jio r o) m = if currentValue r m == 1 then jump o m else next m

compute :: Memory -> Registers
compute memory = 
    case v of
        Just x -> compute (computeInstruction x memory)
        Nothing -> registers memory
    where v = program memory V.!? programCounter memory

initMemory :: [Instruction] -> Memory
initMemory lst = Memory r 0 $ V.fromList lst
    where r = A.listArray (A,B) [1,0]

main :: IO()
main = 
    (\x -> (compute . initMemory) <$> parseFile x) <$> readFile "inputDay23.txt" >>= print