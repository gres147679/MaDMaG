module Contexto where

import Input

type Contexto = [ ([Evento],Float) ]

type MapaContexto = (Contexto,Contexto,Contexto)

removeDuplicates:: (Eq a) =>[a]->[a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

calculaRepeticiones:: (Eq a) => [a] -> a -> Int
calculaRepeticiones listaEvento eventoABuscar= length $ filter (==eventoABuscar) listaEvento

crearContexto1 :: [Evento] -> Contexto
crearContexto1 listaEventos = map (\x -> ([x],fromIntegral(calculaRepeticiones listaEventos x) / fromIntegral(length listaEventos) ) ) (removeDuplicates listaEventos)

crearContexto2 :: [Evento] -> Contexto
crearContexto2 listaEventos = map (\x -> (x,fromIntegral(calculaRepeticiones todo x) / fromIntegral(length todo) ) ) (removeDuplicates todo)
	where 
		todo = (zipWith f listaEventos (tail listaEventos))
		f x y = [x,y]

