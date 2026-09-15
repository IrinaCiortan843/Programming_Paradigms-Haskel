module Algorithms where

import StandardGraph
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Graph a = StandardGraph a

{-
*** TODO 7 (30p) ***

Utilizând equational reasoning, rafinați implementarea dfs din etapa 1
pentru a obține implementarea eficientă bazată pe o stivă.

Pentru aceasta, generalizați funcția internă folosită în etapa 1 (să o numim 
search), astfel încât să ia ca parametru nu doar nodul curent din care începe 
parcurgerea, ci o listă de noduri (să numim noua funcție searchList). Mai 
precis, impuneți proprietatea:

searchList :: Ord a => [a] -> [a]
searchList nodes = concatMap search nodes,

de unde rezultă că

search node = searchList [node].

De aici, derivați o nouă definiție mai eficientă pentru searchList, abordând 
cazul de bază și cazul general.

DERIVARE: ...
caz 1: [] ceva -> lista vida
caz 2: (element : rest) noduri_vizitate
        pas1-> verific faca elem a fost vizitat 
           daca da -> il sar si merg mai departe
           altfel -> il bag in lista de vizitate
                  -> ii iau vecinii : futureVisit
                  -> ii adaug la restul stivei pe care urmeaza sa o vizitez 
        pas2 -> elem + apel de noua_stiva vizitat+elem_curent (care e si el vizitat acum )
Exemple:

>>> dfsStack 1 tree
[1,2,7,15,8,3,9,10,4,11,12,5,13,14,6]

>>> dfsStack 4 tree
[4,11,12]
-}
dfsStack :: Ord a => a -> Graph a -> [a]
dfsStack node graph = go [node] Set.empty
  where 
    go [] _ =[]
    go (x : rest) visited = if Set.member x visited 
                            then go rest visited
                            else
                                let 
                                    newVisited = Set.insert x visited -- il fac vizitat 
                                    futureVisit = Set.toList (outNeighbors x graph) -- iau vecinii
                                    newList = futureVisit ++ rest -- noua lista contine vecinii si ce era inainte 
                                    in
                                       x : go newList newVisited -- il adaug la rezultat si merg mai departe 

{-
DEBUG

Funcții pentru debugging, care afișează un mesaj DEBUG de fiecare dată când
un element este implicat într-o operație de concatenare.

La fel ca (++), appendDebug este asociativă la dreapta în expresii de forma:

xs `appendDebug` ys `appendDebug` zs.

De exemplu, expresiile echivalente asociate la dreapta

[1,2] `appendDebug` ([3] `appendDebug` [4,5,6])
[1,2] `appendDebug`  [3] `appendDebug` [4,5,6]   (implicit la dreapta)

afișează utilizări unice ale elementelor:

[
DEBUG: 1
1,
DEBUG: 2
2,
DEBUG: 3
3,4,5,6],

în timp ce asocierea la stânga

([1,2] `appendDebug` [3]) `appendDebug` [4,5,6]

necesită utilizări repetate ale elementelor:

[
DEBUG: 1

DEBUG: 1
1,
DEBUG: 2

DEBUG: 2
2,
DEBUG: 3
3,4,5,6]
-}
infixr 5 `appendDebug`
appendDebug :: Show a => [a] -> [a] -> [a]
appendDebug xs ys = foldr (\x -> (trace ("\nDEBUG: " ++ show x) x :)) ys xs

concatMapDebug :: Show b => (a -> [b]) -> [a] -> [b]
concatMapDebug f = foldr (appendDebug . f) []
