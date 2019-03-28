module Semantics where

import Syntax

type IntF a = Nombre -> [a] -> a    -- Interpretación de una función
type IntR a = Nombre -> [a] -> Bool -- Interpretación de una relación (predicado)
type Estado a = Ind -> a            -- sigma: Var -> M

type Estructura a = ([a], IntF a, IntR a)   --  L-Estructura

actEst :: Estado a -> Ind -> a -> Estado a
actEst e x n = ne
  where ne y = if x == y then n else e x

iTerm :: Estado a -> IntF a -> Term -> a
iTerm e iF t = case t of
  V x -> e x
  F t lt -> iF t [iTerm e iF t | t <- lt] -- La interpretación de la lista de términos.


iForm :: Eq a => Estructura a -> Estado a -> Form -> Bool
iForm str e phi = case phi of   -- str -> estructura
  FalseF -> False
  TrueF -> True
  Pr p lt -> iR p (map (iTerm e iF) lt) where (_,iF,iR) = str
  Eq t1 t2 -> (iTerm e iF t1) == (iTerm e iF t2) where (_,iF,_) = str
  Neg p -> not (iForm str e p)
  Conj p q -> (iForm str e p) && (iForm str e q)  -- La interpretación de los dos es la misma
  Disy p q -> (iForm str e p) || (iForm str e q)
  Imp p q -> iForm str e (Disy (Neg p) q)
  Equiv p q -> iForm str e (Conj (Imp p q) (Imp q p))
  All x p -> and ([iForm str (actEst e x m) p | m <- u]) where (u,_,_) = str   -- Verificamos que la sustitución se cumpla para cada uno
  Ex x p -> or ([iForm str (actEst e x m) p | m <- u]) where (u,_,_) = str


satForm :: Eq a => Estructura a -> Estado a -> Form -> Bool
satForm str e phi = iForm str e phi
