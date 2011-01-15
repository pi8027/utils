
module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec hiding (satisfy)
import Data.Attoparsec.Char8
import Control.Applicative
import System.Process

acpiParser :: Parser (Int, Int, Int, Int)
acpiParser = do
    p <- string (BS.pack "Battery ") >> decimal >>
        string (BS.pack ": Discharging, ") >> decimal
    t1 <- string (BS.pack "%, ") >> decimal
    t2 <- satisfy (':' ==) >> decimal
    t3 <- satisfy (':' ==) >> decimal
    string (BS.pack " remaining\n")
    return (p, t1, t2, t3)

main :: IO ()
main = do
    acpiResult <- BS.pack <$> readProcess "acpi" [] ""
    case parse acpiParser acpiResult of
        Done _ (p, t1, t2, t3) -> BS.putStr $ BS.pack $
            "(bat: " ++ show p ++ "%, " ++ show t1 ++ ":" ++ show t2 ++ ") "
        _ -> return ()

