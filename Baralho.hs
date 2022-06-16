module Baralho
( Carta (..)
, Baralho (..) 
, baralho40
, baralhar
, darMaos
, reporCarta
, ultima
, dimensao
) where 

import System.Random
import Data.List

naipes = ['♣', '♦', '♥', '♠']
valores = ['A', '2', '3', '4', '5', '6', '7', 'Q', 'J' ,'K'] 
ordValores = [ '2', '3', '4', '5', '6', 'Q', 'J' ,'K','7', 'A'] 

data Carta = Carta {  valor :: Char 
                    , naipe :: Char} 

instance Show Carta where 
 show (Carta valor naipe) = valor :  [naipe]

instance Eq Carta where 
 (==) = equalsCarta

equalsCarta :: Carta -> Carta -> Bool
equalsCarta (Carta v1 n1) (Carta v2 n2) = v1 == v2 && n1 == n2 


instance Ord Carta where
 (>) (Carta v1 n1 ) (Carta v2 n2 )  = if (elemIndex n1 naipes) > (elemIndex n2 naipes) then True else (elemIndex n1 naipes) == (elemIndex n2 naipes) && (elemIndex v1 ordValores) > (elemIndex v2 ordValores)

 (>=) (Carta v1 n1 ) (Carta v2 n2 ) = if (elemIndex n1 naipes) >= (elemIndex n2 naipes) then (elemIndex v1 ordValores) >= (elemIndex v2 ordValores) else False

 (<) (Carta v1 n1 ) (Carta v2 n2 ) =  if (elemIndex n1 naipes) < (elemIndex n2 naipes) then True else (elemIndex n1 naipes) == (elemIndex n2 naipes) && (elemIndex n1 ordValores) < (elemIndex n2 ordValores)

 (<=) (Carta v1 n1 ) (Carta v2 n2 ) = if (elemIndex n1 naipes) <= (elemIndex n2 naipes) then  (elemIndex v1 ordValores) <= (elemIndex v2 ordValores) else False

data Baralho = Baralho { cartas :: [Carta] }

instance Show Baralho where
 show (Baralho xs) = tail ( foldr (\ x acc-> "," ++ show x ++ acc) "" xs )

baralho40:: Baralho
baralho40  = (Baralho [Carta x y | y<- naipes, x <- valores] )

baralhar :: StdGen -> Baralho -> Baralho
baralhar ng (Baralho xs) = Baralho (map snd $ sort $ zip indices xs)
  where indices = randomRs (1, length xs ^ 3) ng :: [Int]


darMaos :: Int -> Int -> Baralho -> ([[Carta]],Baralho)
darMaos nJogadores nCartas (Baralho xs) 
   | numeroCartas > length xs   = ([], (Baralho xs) )
   | otherwise = (transpose $ chunks nJogadores cartas,(Baralho  (drop (numeroCartas) xs )) ) 
                      where numeroCartas = nJogadores * nCartas 
                            cartas = take numeroCartas xs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

----------
reporCarta :: Carta -> Baralho -> Baralho
reporCarta x (Baralho xs ) = (Baralho (x:xs))  

ultima :: Baralho -> Carta
ultima (Baralho xs) = last xs

dimensao :: Baralho -> Int
dimensao (Baralho xs) = length xs
























