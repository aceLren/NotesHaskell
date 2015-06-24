{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Data.Time hiding (parseTime)

data IP = IP Word8 Word8 Word8 Word8 deriving Show
data Product = Mouse | Keyboard | Monitor | Speakers deriving (Show,Enum)
data LogEntry = LogEntry
    { entryTime :: LocalTime
    , entryIP   :: IP
    , entryProd :: Product
    } deriving Show

productFromID :: Int -> Product
productFromID n = toEnum (n - 1)

productToID :: Product -> Int
productToID p = fromEnum p + 1

type Log = [LogEntry]

parseIP :: Parser IP
parseIP = do
    d1 <- decimal
    char '.'
    d2 <- decimal
    char '.'
    d3 <- decimal
    char '.'
    d4 <- decimal
    return $ IP d1 d2 d3 d4

parseTime :: Parser LocalTime
parseTime = do
    y <- count 4 digit
    char '-'
    mm <- decimal
    char '-'
    d <- decimal
    skipSpace
    h <- decimal
    char ':'
    m <- decimal
    char ':'
    s <- decimal

    return $ LocalTime
        {localDay = fromGregorian (read y) mm d
        ,localTimeOfDay = TimeOfDay h m (fromIntegral s)
        }

parseProduct :: Parser Product
parseProduct = productFromID . read . (:[]) <$> digit

parseAll :: Parser LogEntry
parseAll = do
    tm <- parseTime
    skipSpace
    ip <- parseIP
    skipSpace
    pr <- parseProduct

    return $ LogEntry tm ip pr

main :: IO ()
main = do
    -- This forces parser to consume all input
    print $ parseOnly (parseTime <* endOfInput) "1995-11-1    1:24:35"
    print $ parseOnly parseIP "198.168.1.1     asdfkj asd;lkj "
    print $ parseOnly parseProduct "2"
    print $ parseOnly parseAll "1997-1-12  03:2:31   1.245.3.523   1"

