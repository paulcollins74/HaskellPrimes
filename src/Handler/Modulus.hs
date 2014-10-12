module Handler.Modulus where

import Import
import Data.Bits

-- odd :: Integral a => a -> Bool
-- odd n = n `mod` 2 == 1

toBinary :: Integer -> [Bool]
toBinary 0 = []
toBinary n = odd n : toBinary (n `shiftR` 1)

showBin :: [Bool] -> String
showBin [] = ""
showBin (x:xs) | x = showBin xs ++ "1"
showBin (_:xs) = showBin xs ++ "0"


getBinR :: Integer -> Handler Value
getBinR i = return $ object ["value" .= showBin (toBinary i) ]


testMain :: IO ()
testMain = do
    let bs = toBinary 561
    print bs
