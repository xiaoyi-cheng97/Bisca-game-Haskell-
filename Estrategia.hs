{-|
Module		: Estrategia
Description 	: Estratégias disponíveis para jogar a Bisca
Copyright	: (c) Alcides Fonseca, 2017
			Vasco T. Vasconcelos, 2017
License		: GPL-3
Maintainer 	: docentes-pp@listas.di.ciencias.ulisboa.pt
Stability	: experimental
Portability 	: POSIX

Este módulo exporta o tipo Estratégia e uma função que
seleciona uma estratégia a um determinado ponto. Pode
ser usado em jogos de trick-taking como o Burro, a
Bisca ou a Sueca.
-}

module Estrategia
( Estrategia
, estrategia
) where

import Baralho

{-| Uma estratégia consiste em escolher uma carta dada
uma mão de cartas e as cartas na mesa. Assume que a mã
o tem pelo menos uma carta. -}

type Estrategia = [Carta] -> [Carta] -> Carta

{-| Escolher sempre a primeira carta na mão. -}
primeiraCarta :: Estrategia
primeiraCarta (carta:_) _ = carta

{-| Começar com a pior carta, continuar com a melhor
carta. -}
piorPrimeiro :: Estrategia
piorPrimeiro mao [] = minimum mao
piorPrimeiro mao _ = maximum mao

{-| Começar com a melhor carta, continuar com a pior
carta. -}
melhorPrimeiro :: Estrategia
melhorPrimeiro mao [] = maximum mao
melhorPrimeiro mao _ = minimum mao

{-| Estratégia para um dado jogador. -}
estrategia :: Int -> Estrategia
estrategia n = [primeiraCarta, piorPrimeiro,melhorPrimeiro] !! (n `mod` 3)