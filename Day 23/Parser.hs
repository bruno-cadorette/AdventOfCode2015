{-# LANGUAGE OverloadedStrings #-}
module Parser (parseFile, Instruction(..), Register(..)) where
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8(pack)
import Control.Applicative
import Control.Monad
import Data.Ix

data Register = A | B deriving (Show, Enum, Ord, Eq, Ix)
type Offset = Int
data Instruction = Hlf Register | Tpl Register | Inc Register | Jmp Offset | Jie Register Offset | Jio Register Offset deriving (Show)

registerParser :: Parser Register
registerParser = (char 'a' >> return A) <|> (char 'b' >> return B)

registerInstructionParser :: Parser Instruction
registerInstructionParser = do
    instruction <-  (string "hlf" >> return Hlf) <|>
                    (string "tpl" >> return Tpl) <|>
                    (string "inc" >> return Inc)
    void space
    register    <- registerParser
    return $ instruction register
    
offsetParser :: Parser Int
offsetParser = do
    f <- (char '+' >> return id) <|> (char '-' >> return negate)
    offset <- decimal
    return $ f offset
    
multiArgumentInstructionParser :: Parser Instruction
multiArgumentInstructionParser = do
    instruction <-  (string "jie" >> return Jie) <|>
                    (string "jio" >> return Jio)
    void space
    register <- registerParser
    void $ string ", "
    offset <- offsetParser
    return $ instruction register offset
    
offsetInstructionParser :: Parser Instruction
offsetInstructionParser = do
    instruction <- string "jmp" >> return Jmp
    void space
    offset <- offsetParser
    return $ instruction offset
    
parseFile :: String -> Either String [Instruction]
parseFile = parseOnly parser . pack
    where parser =  many $ (offsetInstructionParser <|> multiArgumentInstructionParser <|> registerInstructionParser)  <* endOfLine
    