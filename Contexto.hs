module Contexto where

import Input
import Data.Map

type Contexto = [ ([Evento],Float) ]

type MapaContexto = (Contexto,Contexto,Contexto)

removeDuplicates:: (Eq a) =>[a]->[a]
removeDuplicates = Prelude.foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

calculaRepeticiones:: (Eq a) => [a] -> a -> Int
calculaRepeticiones listaEvento eventoABuscar= length $ Prelude.filter (==eventoABuscar) listaEvento

crearContexto1 :: [Evento] -> Contexto
crearContexto1 listaEventos = Prelude.map (\x -> ([x],fromIntegral(calculaRepeticiones listaEventos x) / fromIntegral(length listaEventos) ) ) (removeDuplicates listaEventos)

crearContexto2 :: [Evento] -> Contexto
crearContexto2 listaEventos = Prelude.map (\x -> (x,fromIntegral(calculaRepeticiones todo x) / fromIntegral(length todo) ) ) (removeDuplicates todo)
	where 
		todo = (zipWith f listaEventos (tail listaEventos))
		f x y = [x,y]

construirMapaRepeticiones:: [[Evento]] -> (Data.Map.Map [Evento] Int)
construirMapaRepeticiones lista = fromList $ zip lista (Prelude.map (calculaRepeticiones lista) lista)

--construirMapaRepeticiones':: [Evento] -> Data.Map.Map Evento Int
--construirMapaRepeticiones' lista = fromList [(x,y) | x <- lista , y <- [(calculaRepeticiones lista x)]]

distanciaEntreModelos :: (Data.Map.Map [Evento] Int) -> (Data.Map.Map [Evento] Int) -> Int
distanciaEntreModelos pieza1 pieza2 = sqrt $ fromIntegral $ sum $ Prelude.map (\x -> ((pieza1 ! x) - (pieza2 ! x))^2 ) (keys pieza1)