module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

data Personaje = Personaje {
    nombre :: String,
    poderBasico :: String,
    superPoder :: String,
    superActivo :: Bool,
    vida :: Int,
    vidaInicial :: Int
} deriving Show

Espina :: Personaje
Espina = ("Espina", "bola de espinas", "granadas de espinas", True, 4800, 4800)

Pamela :: Personaje
Pamela = ("Pamela", "lluvia de tuercas", "torreta Curativa", False, 9600, 9600)

curar :: Personaje -> Int -> Personaje
curar personaje sanacion = personaje {vida = min (vida personaje + sanacion) (vidaInicial personaje)}

herir :: Personaje -> Int -> Personaje
herir personaje danio = personaje {vida = max 0 (vida personaje - danio)}

bolaEspinosa :: Personaje -> Personaje
bolaEspinosa personaje = herir personaje 1000

amigo :: String -> Bool
amigo amigoOenemigo = amigoOenemigo == "colega"

lluviaDeTuercas :: Personaje -> String -> Int
lluviaDeTuercas personaje amigoOenemigo = 
  |amigo amigoOenemigo = curar personaje 800
  |otherwise = herir personaje 800



  