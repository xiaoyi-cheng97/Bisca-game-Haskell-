module JogarBisca 
( jogarBisca ) where


import Baralho
import Estrategia
import Data.List
import System.Random
import Control.Monad

pontuacao :: Carta -> Int
pontuacao c | valor c == 'A' = 11
            | valor c == '7' = 10
            | valor c == 'K' = 4
            | valor c == 'J' = 3
            | valor c == 'Q' = 2
            |otherwise = 0

mesa :: [[Carta]] -> [String]
mesa maos = ["Mao do jogador " ++ show y ++ ": " ++ show (maos !! y) | y <- [0..(length maos) -1]]

quemGanha :: [(Int, Carta)] -> Char -> Int
quemGanha v trunfo
	|not $ null xs = js !! ((elemIndices ( maximum xs ) cs) !! 0) 
	|otherwise = js !! ((elemIndices (maximum cs) cs) !! 0)
              where (js, cs) = unzip v
		    xs = haTrunfo cs trunfo

haTrunfo :: [Carta] -> Char -> [Carta]
haTrunfo cartas trunfo = [x | x <- cartas,  naipe x == trunfo]

cartasJogada :: [[Carta]] -> Int -> [Carta]
cartasJogada maos quemComeca = (primeiraCarta): [estrategia x (maosEmOrdem!! x) [primeiraCarta] | x <- [1..(length maos) -1]]
                               where maosEmOrdem = take (length maos) (drop quemComeca (cycle maos))
                                     primeiraCarta = estrategia 0 (maosEmOrdem !! 0) []

ultimaVaza :: [Carta] -> Int -> Int -> [(Int, Carta)]
ultimaVaza cartasJogada quemComeca nJogadores = zip (take nJogadores (drop quemComeca (cycle [0..nJogadores-1]))) cartasJogada


novaPontuacao:: [Int] -> [Carta] -> Int -> [Int]
novaPontuacao historico pot quemGanhou = (take quemGanhou historico) ++ ((historico !! quemGanhou) +(sum (map pontuacao pot)) : drop (quemGanhou + 1) historico)

comecaJogo :: [[Carta]] -> Baralho -> IO()
comecaJogo mao monte = do          
                                        putStrLn "--- Mesa ---"
                                        -- função recursiva que imprime mao dos jogadores
                                        mapM putStrLn (mesa mao) 
                                        -- cartas que foram jogadas
                                        putStrLn "Última vaza:"
                                        --Monte
                                        putStrLn $ "Monte: " ++ show monte
                                        --jogador que ganha
                                        putStrLn $ "Proximo jogador: 0"

auxJogarBisca :: [[Carta]] -> Baralho -> Char -> Int -> [Int] -> IO [Int]
auxJogarBisca maos monte trunfo quemComeca pontos = if (null (head maos)  ) then do
                                                          putStrLn ""
                                                          return $ pontos
                                                       else
                                                         do
                                                            let pot = cartasJogada maos quemComeca 
                                                            let vaza = ultimaVaza pot quemComeca (length maos)
                                                            let maoJogada = [delete (pot !! x) (maosEmOrdem!! x) | x <- [0..(length maos) -1] ]  
									where maosEmOrdem = take (length maos) (drop quemComeca (cycle maos))
                                                            
							    let (dar, novoMonte) = darMaos (length maos) (3 - (length (head maoJogada))) monte
                                                            let novaMao = if null dar then maoJogada else take (length maos) (drop ((length maos)-quemComeca) (cycle [cartas $ reporCarta (head (dar!!x )) (Baralho (maoJogada !! x)) | x <- [0 ..(length maos) -1]]))
                                                                       

							    putStrLn "--- Mesa ---"
                                                            mapM putStrLn (mesa novaMao)
                                                            putStrLn $ "Última vaza:" ++ show vaza
                                                            putStrLn $ "Monte: " ++ if  null $ cartas novoMonte then "" else show novoMonte
                                                            let proximoJ = quemGanha vaza trunfo
                                                            putStrLn $ "Proximo jogador: " ++ show proximoJ
                                                            auxJogarBisca novaMao novoMonte trunfo proximoJ (novaPontuacao pontos pot proximoJ)
                                                            

                                         



jogarBisca :: Int -> Baralho -> IO[Int]
jogarBisca nJogadores baralho = do   let trunfo = naipe (ultima baralho)
                                     let (maos, b) = darMaos nJogadores 3 baralho
                                     putStrLn $ "Trunfo: " ++ [trunfo]
                                     comecaJogo maos b
                                     auxJogarBisca maos b trunfo 0 (replicate nJogadores 0)
                             
                                  
                             

