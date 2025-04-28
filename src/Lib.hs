module Lib () where

import Text.Show.Functions ()

doble :: Int -> Int
doble x = x * 2

type Personaje=(String, String, String, Bool, Int)

Espina :: Personaje
Espina = "Espina", "bola de espinas", "granadas de espinas", True, 4800

Pamela :: Personaje
Pamela = "Pamela" "lluvia de tuercas" "torreta Curativa" False 9600

vidaDePersonaje :: [Personaje] -> Int
vidaDePersonaje (_, _, _, _, vida) = vida

vidaEsMenorA :: [Personaje] -> Int -> Bool
vidaEsMenorA personaje danio = (vidaDePersonaje personaje) < danio

bolaEspinosa :: [Personaje]->Int
bolaEspinosa personaje 
  |vidaEsMenorA personaje 1000 = vidaDePersonaje personaje = 0
  |otherwise (vidaDePersonaje personaje) - 1000 