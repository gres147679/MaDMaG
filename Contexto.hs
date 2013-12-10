module Contexto where

import Input

import Data.List (unfoldr)

import Data.Map (Map, fromList, keys, (!) )

import System.Random

type Contexto = [ ([Evento],Float) ]

type MapaContexto = [Contexto]

removeDuplicates :: (Eq a) =>[a]->[a]
removeDuplicates = Prelude.foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

buscarProbabilidad:: [Evento] -> Contexto -> Float
buscarProbabilidad aBuscar miMapa = quitaMaybe (lookup aBuscar miMapa)
	where
		quitaMaybe :: Maybe Float -> Float
		quitaMaybe (Just b) = b
		quitaMaybe (Nothing) = fromIntegral(0)



calculaRepeticiones:: (Eq a) => [a] -> a -> Int
calculaRepeticiones listaEvento eventoABuscar= length $ Prelude.filter (==eventoABuscar) listaEvento

crearContexto1 :: [Evento] -> Contexto
crearContexto1 listaEventos = Prelude.map (\x -> ([x],fromIntegral(calculaRepeticiones listaEventos x) / fromIntegral(length listaEventos) ) ) (removeDuplicates listaEventos)

crearContexto2 :: [Evento] -> Contexto -> Contexto
crearContexto2 listaEventos orden1 = Prelude.map (\x -> (x, 0.3*(buscarProbabilidad [last x] orden1)+ 0.7*(fromIntegral(calculaRepeticiones todo x) / fromIntegral(calculaRepeticiones listaEventos (head x) )) ) ) (removeDuplicates todo)
	where 
		todo = (zipWith f listaEventos (tail listaEventos))
		f x y = [x,y]

crearMapaContexto :: [Evento] -> MapaContexto
crearMapaContexto listaEventos = [ [([],fromIntegral (length listaEventos))],
								contexto1,normalizar (crearContexto2 listaEventos contexto1)]
	where
		contexto1 = crearContexto1 listaEventos

construirMapaRepeticiones:: [[Evento]] -> (Data.Map.Map [Evento] Int)
construirMapaRepeticiones lista = fromList $ zip lista (Prelude.map (calculaRepeticiones lista) lista)

--construirMapaRepeticiones':: [Evento] -> Data.Map.Map Evento Int
--construirMapaRepeticiones' lista = fromList [(x,y) | x <- lista , y <- [(calculaRepeticiones lista x)]]

distanciaEntreModelos :: (Data.Map.Map [Evento] Integer) -> (Data.Map.Map [Evento] Integer) -> Float
distanciaEntreModelos pieza1 pieza2 = sqrt $ fromInteger $ sum $ Prelude.map (\x -> ((pieza1 ! x) - (pieza2 ! x))^2 ) (keys pieza1)

buscarElementoOrden2 :: [Evento] -> Contexto -> Contexto
buscarElementoOrden2 aBuscar contexto = filter f contexto
	where
		f x = (head (fst x)) == (head aBuscar)


siguienteEvento :: Contexto -> Float -> Evento
siguienteEvento contexto proba 
	| proba > 1.0 = error "Pajuo3"
	| otherwise = siguiente contexto proba 0.0
	where
		siguiente [] _ _ = error "Pajuo"
		siguiente (x:contex) prob acum = 
			if (acum + (snd x) )>=prob
			then last $ fst x
			else siguiente contex prob (acum + (snd x))


ev :: Int -> Int -> (Int,Int)
ev x y = (x,y)

f1 :: Float -> Float
f1 x = x

normalizar :: Contexto -> Contexto
normalizar micontexto = map (\x -> (fst x,(snd x) / sumaTotal) ) micontexto
	where
		sumaTotal = sum ( map snd micontexto)


randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . (randomR (0,99)))


toFloatArray :: [Int] -> [Float]
toFloatArray = map fromIntegral 

generarCancion :: Int -> [Int] -> MapaContexto -> [Evento]
generarCancion longitud randoms miMapa = map fst $ take longitud $ iterate f ((0,0),0)
	where
		f (elEvento,index)
			| elEvento == (0,0) = do
				(siguienteEvento (miMapa !! 1) miRandom,index+1)
			| (buscarElementoOrden2 [elEvento] (miMapa !! 2) ) == [] = (siguienteEvento (miMapa !! 1) miRandom,index+1)
			| otherwise = (siguienteEvento (normalizar (buscarElementoOrden2 [elEvento] (miMapa !! 2) )) miRandom,index+1)
			where
				miRandom = fromIntegral(randoms !! index) / 100