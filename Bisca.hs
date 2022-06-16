import Baralho
import Estrategia
import JogarBisca
import System.Random
import System.Environment
import System.IO
import Data.Char

pontuacoes :: [Int] -> [String]
pontuacoes pontos =  ["Jogador " ++ show y ++ " obteve " ++ show(pontos !! y) | y <- [0..(length pontos) -1]]

main :: IO()
main = do 
        nJ <- getArgs
        g <- newStdGen
        pontos <- jogarBisca (read (nJ!!0)::Int) (baralhar g baralho40)
        putStrLn "--- Jogo terminado ---" 
        mapM putStrLn (pontuacoes  pontos)
        return ()

