module AlgMM where

import Syntax
import Data.List


--Simplifica una sustitución.
simpSus :: Subst -> Subst
simpSus sus = [(x, t) | (x, t) <- sus, V x /= t]  -- Verifica que nos queden igual
                                                  -- después de la sustitución.


{--
  Tomamos las variables x tales que ya se les aplicó la primera sustitución y
  después les aplicamos la segunda sustitución y quitamos aquellos que están
  repetidos.

  ws checamos que las variables de la segunda sustitución no estén el las variables
  de la primera.
--}
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = zs ++ ws
  where zs = simpSus [(x, apsubT t s2) | (x, t) <- s1] -- Pares de la primera sustitución y les aplicamos la segunda.
        ws = [ (x, t) | (x, t) <- s2, not (elem x vs1)]
        vs1 = fst (unzip s1) -- devuelve las variables de la primer sustitución
