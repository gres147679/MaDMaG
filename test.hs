import Input
import Contexto

main = do 	
	(seqs,names) <- loadMusicXmls "./xml/"
	-- a <- crearContexto1 (take 1)
	return () 