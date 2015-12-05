{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash.MD5
import Data.List
import Data.ByteString.Base16
import qualified Data.ByteString.Char8 as B

key = "yzbqklnj"

isValid :: Int -> Int -> Bool
isValid n i = possibleResult == (B.take n $ getHashString i)
    where possibleResult = B.replicate n '0'
    
showB :: Show a => a -> B.ByteString
showB = B.pack . show

getHashString :: Show a => a -> B.ByteString
getHashString  = encode . hash . B.append key . showB 

day4A = find (isValid 5) [1..]
day4B = find (isValid 6) [1..]

main = do
    print day4A
    print day4B