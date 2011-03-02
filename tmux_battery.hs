
module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Char8
import Control.Applicative
import Control.Monad
import System.Process

data BatteryState =
    BatCharging Int Int Int Int | BatDischarging Int Int Int Int | BatFull Int

acpiParser :: Parser BatteryState
acpiParser = string (BS.pack "Battery ") >>
    decimal >> fullP <|> chargingP <|> dischargingP where
    fullP = string (BS.pack ": Full, ") *>
        liftM BatFull decimal <* string (BS.pack "%\n")
    chargingP = string (BS.pack ": Charging, ") *>
        moreStateP BatCharging <* string (BS.pack " until charged\n")
    dischargingP = string (BS.pack ": Discharging, ") *>
        moreStateP BatDischarging <* string (BS.pack " remaining\n")
    moreStateP f = liftM4 f
        decimal (string (BS.pack "%, ") >> decimal)
        (satisfy (':' ==) >> decimal) (satisfy (':' ==) >> decimal)

main :: IO ()
main = readProcess "acpi" [] "" >>=
    putStr . strResult . parse acpiParser . BS.pack where
    strResult (Done _ (BatCharging p t1 t2 t3)) =
        "(bat+: " ++ show p ++ "%, " ++ show t1 ++ ":" ++ show t2 ++ ") "
    strResult (Done _ (BatDischarging p t1 t2 t3)) =
        "(bat-: " ++ show p ++ "%, " ++ show t1 ++ ":" ++ show t2 ++ ") "
    strResult (Done _ (BatFull n)) = "(bat: full) "
    strResult _ = ""

