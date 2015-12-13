{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Bits
import Data.ByteString.Char8 (pack)
import Data.Word
import Data.Function.Memoize
import qualified Data.Map as Map

type Variable = String
data BitAction = And | Or | LShift | RShift deriving (Show, Ord, Eq)
data Function = Value Data | Not Data | BiFunction BitAction Data Data deriving (Show, Ord, Eq)
data Data = Number Word16 | Wire Variable | Func Function deriving (Show, Ord, Eq)
data Instruction = Instruction Function Variable deriving (Show, Ord, Eq)

type Diagram = Map.Map Variable Function

biActionParser = 
    (string "AND" >> return And) <|>
    (string "OR" >> return Or) <|>
    (string "LSHIFT"  >> return LShift) <|>
    (string "RSHIFT"  >> return RShift)
    
variableParser = many letter_ascii
    
dataParser = 
    (Number <$> decimal) <|>
    (Wire <$> variableParser)

biFunctionParser = do
    a <- dataParser
    space
    f <- biActionParser
    space
    b <- dataParser
    return $ BiFunction f a b
    
functionParser = 
    (string "NOT" >> space >> dataParser >>= return . Not) <|>
    biFunctionParser <|> 
    (Value <$> dataParser)

instructionParser = do
    function <- functionParser
    string " -> "
    result <- variableParser
    return $ Instruction function result
    
parseFile = fmap Map.fromList . mapM (fmap instructionToTuple . parseOnly instructionParser . pack) . lines 
    where instructionToTuple (Instruction function variable) = (variable, function)

getValue :: Variable -> Diagram -> Word16
getValue start diagram = mapValue (Wire start)
    where 
        getWireValue = memoize (\v-> executeFunction $ diagram Map.! v)
        mapValue :: Data -> Word16
        mapValue (Number value) = value
        mapValue (Wire value) = getWireValue value
        mapValue (Func function) = executeFunction function
        
        executeFunction :: Function -> Word16
        executeFunction (Value v) = mapValue v
        executeFunction (Not v) = complement $ mapValue v
        executeFunction (BiFunction function a b) = toRealAction function (mapValue a) (mapValue b)
    
toRealAction :: (Integral a, Bits a) => BitAction -> a -> a -> a 
toRealAction And    = (.&.)
toRealAction Or     = (.|.)
toRealAction LShift = \a b -> shiftL a (fromIntegral b)
toRealAction RShift = \a b -> shiftR a (fromIntegral b)
    
main = do 
    file <- readFile "inputDay7.txt"
    print $ (getValue "a") <$> parseFile file
    print $ (getValue "a" . Map.insert "b" (Value (Number 956))) <$> parseFile file