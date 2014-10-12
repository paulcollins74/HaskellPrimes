module Handler.Modulus where

import Import
import Data.Bits
import System.Random

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

getTest1R :: Integer -> Handler Value
getTest1R i = return $ object ["value" .= show (testRandom $ fromInteger i) ]

getTest2R :: Integer -> Integer -> Handler Value
getTest2R i j = return $ object ["value" .= show (callMillerRabin i j) ]

getTest3R :: Integer -> Integer -> Integer -> Handler Value
getTest3R i j k = return $ object ["value" .= show (modExponentTest i j k) ]

modExponent :: Integer -> Integer -> Integer -> Integer
modExponent a b n = snd $ foldl (loop1 a n) (0,1) bs
    where
    bs = reverse $ toBinary b

modExponentTest :: Integer -> Integer -> Integer -> [(Integer,Integer)]
modExponentTest a b n = scanl (loop1 a n) (0,1) bs
    where
    bs = reverse $ toBinary b

decompose :: (Integer,Integer) -> (Integer,Integer)
decompose (t,n) | odd n  = (t,n)
decompose (t,n) | even n = decompose (t+1,n `div` 2)

-- If returns True then number is Composite.  If False then number may be prime
witness :: Integer -> Integer -> Bool
witness a n = xt /= 1
    where
    (t,u) = decompose (0,n-1)
    x0 = modExponent a u n
    (xt,_,_) = until (\(_,_,b) -> b) (loop2 n) (x0,t,False)

-- If returns True then number is Composite.  If False then number is almost certainly prime
callMillerRabin :: Integer -> Integer -> Bool
callMillerRabin n s = b
    where
    (_,b,_) = until (\(t,b,_) -> t == 0 || b) (millerRabin n) (s,False,mkStdGen 3)

millerRabin :: Integer -> (Integer,Bool,StdGen) -> (Integer,Bool,StdGen)
millerRabin n (s,b,g) | s == 0 || b = (0,True,g)
millerRabin n (s,False,g) = (s-1, witness (toInteger a) n, g1)
    where
    (a,g1) = randomR (1,n-1) g

testRandom :: Int -> [Int]
testRandom s = map fst $ scanl (\(n,g) _ -> randomR (1,100) g) (0,mkStdGen 3) [1..s]

loop2 :: Integer -> (Integer,Integer,Bool) -> (Integer,Integer,Bool)
loop2 n (x,0,_) = (x,0,True)
loop2 n (x,t,_) = (y,t-1,b)
    where
    y = x*x `mod` n
    b = y == 1 && x /= 1 && x /= n-1


loop1 :: Integer -> Integer -> (Integer,Integer) -> Bool -> (Integer,Integer)
loop1 a n (c,d) True = (2*c+1,d*d*a `mod` n)
loop1 a n (c,d) False = (2*c,d*d `mod` n)

testMain :: IO ()
testMain = do
    let bs = toBinary 561
    print bs
