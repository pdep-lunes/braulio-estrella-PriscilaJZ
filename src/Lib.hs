module Lib (Personaje(..), espina, pamela, personajes, curar, herir, bolaEspinosa, amigo, lluviaDeTuercas,
 torretaCurativa, granadaDeEspinas, esVidaMenorA800, quienTieneMenosDe800HP) where

import Text.Show.Functions ()

--doble :: Int -> Int
--doble x = x * 2

data Personaje = Personaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superActivo :: Bool,
    vida :: Int,
    vidaInicial :: Int
} deriving Show

espina :: Personaje
espina = Personaje "Espina" "bola de espinas" "granadas de espinas" True 4800 4800

pamela :: Personaje
pamela = Personaje "Pamela" "lluvia de tuercas" "torreta curativa" False 9600 9600

personajes :: [Personaje]
personajes = [pamela, espina]

curar :: Personaje -> Int -> Personaje
curar personaje sanacion = personaje {vida = min (vida personaje + sanacion) (vidaInicial personaje)}

herir :: Personaje -> Int -> Personaje
herir personaje danio = personaje {vida = max 0 (vida personaje - danio)}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = herir personaje 1000

amigo :: String -> Bool
amigo amigoOEnemigo = amigoOEnemigo == "colega"

lluviaDeTuercas :: String -> Personaje -> Personaje
lluviaDeTuercas amigoOEnemigo personaje
  | amigo amigoOEnemigo = curar personaje 800
  | otherwise = herir personaje 800

torretaCurativa :: Personaje -> Personaje
torretaCurativa personaje = personaje {superActivo = True, vida = min (vida personaje * 2) (vidaInicial personaje)}

granadaDeEspinas ::  Int -> Personaje -> Personaje
granadaDeEspinas radio personaje 
  | radio > 3 && vida personaje < 800 = personaje {nombre = nombre personaje ++ "Espina estuvo aquí", vida = 0, superActivo = False}
  | radio > 3 = personaje {nombre = nombre personaje ++ "Espina estuvo aquí"}
  | otherwise = bolaEspinosa personaje

esVidaMenorA800 :: Personaje -> Bool
esVidaMenorA800 personaje = vida personaje < 800

quienTieneMenosDe800HP :: [Personaje] -> [Personaje]
quienTieneMenosDe800HP listaDePersonajes = filter esVidaMenorA800 listaDePersonajes

lluviaDeTuercas "enemigo" :: Personaje -> Personaje

ataqueConSuperPoder :: Int -> Personaje -> Personaje -> Personaje
ataqueConSuperPoder radio personaje personajeEnemigo
  | superPoder personaje == "granadas de espinas" = granadaDeEspinas personajeEnemigo radio
  | superPoder personaje == "torreta curativa" = torretaCurativa personajeEnemigo
  | otherwise = personajeEnemigo

ataqueConBasico :: Personaje -> Personaje -> Personaje
ataqueConBasico personaje personajeEnemigo
  | poderBasico personaje == "bola de espinas" = bolaEspinosa personajeEnemigo
  | poderBasico personaje == "lluvia de tuercas" = lluviaDeTuercas "enemigo" personajeEnemigo
  | otherwise = personajeEnemigo 

ataqueConPoderEspecial :: Int -> Personaje -> Personaje -> Personaje
ataqueConPoderEspecial radio personaje personajeEnemigo
  | superActivo personaje == True = ataqueConBasico personaje (ataqueConSuperPoder radio personaje personajeEnemigo)
  | otherwise = personajeEnemigo