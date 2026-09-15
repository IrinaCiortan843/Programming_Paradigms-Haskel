module Modular where

import Data.Set (Set)
import qualified Data.Set as Set
import AlgebraicGraph
import Data.Foldable ( minimumBy )
import Data.Ord ( comparing )

type Graph a = AlgebraicGraph a

-- Graful descris în diagrama din enunțul temei
diagram :: AlgebraicGraph Int
diagram = ((1*2) * (3+4)) * 5

{-
O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
(fără elemente comune) și care împreună conțin toate elementele originale.

De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].
-}
type Partition a = Set (Set a)

{-
*** TODO ***

Funcția mapSingle din etapa 2.
-}
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = 
  let first=[]
      last=xs
      in go first last
      where
        go first []= []
        go first last = (first ++ [f (head last)] ++ tail last)
                :go (first ++ [head last]) (tail last)

{-
*** TODO ***

Funcția partitions din etapa 2.
-}
partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x : xs) = [ mp | p <- partitions xs, 
                             mp <- ([x] : p) : mapSingle (x :) p ]

{-
*** TODO 10 (10p) ***

Implementați funcția isModule, care verifică dacă o mulțime este un modul,
i.e. dacă toate nodurile din mulțime au aceeași mulțime de vecini out și 
aceeași mulțime de vecini in, în exteriorul mulțimii de plecare. Cu alte 
cuvinte, excludem din verificare vecinii din interiorul mulțimii de plecare.

Hint: Set.map poate reduce dimensiunea unei mulțimi dacă elemente diferite
inițial sunt asociate cu același element final, întrucât nu pot exista
duplicate.

Exemple:

>>> isModule (Set.fromList [1,2,3,4]) diagram
True

>>> isModule (Set.fromList [5]) diagram
True

>>> isModule (Set.fromList [1,2]) diagram
True

>>> isModule (Set.fromList [3,4]) diagram
True

>>> isModule (Set.fromList [1,3]) diagram
False
-}
isModule :: Ord a
         => Set a
         -> Graph a
         -> Bool
isModule set graph = 
  let a = Set.findMax set    -- elem random din set
      inN x graph = Set.filter(\z->not (Set.member z set)) (inNeighbors x graph) -- vecinii care nu sunt in set 
      outN x graph = Set.filter(\z->not (Set.member z set)) (outNeighbors x graph) 
      var = Set.filter (\x -> if (inN a graph ==inN x graph && outN a graph==outN x graph)
                              then True
                              else False ) set --vecini nod (a) = vecini nod curent (x)
    in 
      if (var==set )
      then True
      else False 
    

{-
*** TODO 11 (8p) ***

Implementați funcția isModularPartition, care verifică dacă o partiție
a mulțimii de noduri constituie o descompunere modulară. Partiția este 
reprezentată ca o mulțime de mulțimi.

Hint: la fel ca la isModule.

Exemple:

>>> isModularPartition (toPartition [[1], [2], [3], [4], [5]]) diagram
True

>>> isModularPartition (toPartition [[1,2,3,4,5]]) diagram
True

>>> isModularPartition (toPartition [[1,2,3,4], [5]]) diagram
True

>>> isModularPartition (toPartition [[1,2], [3,4], [5]]) diagram
True

>>> isModularPartition (toPartition [[1,3], [2,4,5]]) diagram
False
-}
isModularPartition :: Ord a
                   => Partition a
                   -> Graph a
                   -> Bool
isModularPartition partition graph = -- o partitie 
  let var = Set.filter (\x -> if (isModule x graph==True) -- e modulara ?-> adica fiecare set e un modul 
                              then True
                              else False) partition
    in
      if (var==partition)
      then True
      else False

{-
*** TODO 12 (12p) ***

Implementați funcția maximalModularPartition, care determină partiția maximală 
dintr-o mulțime de partiții. Partiția maximală conține cele mai acoperitoare 
submulțimi ale mulțimii de noduri. Cu alte cuvinte, partiția maximală conține 
cel mai mic număr de submulțimi mai mare strict decât 1, pentru a exlcude 
partiția banală care conține doar întreaga mulțime de noduri.

Hint: minimumBy din Data.Foldable. Funcția este folosită pentru a stabili
un criteriu ad-hoc de ordonare, conform valorii întoarse de o funcție f
când este aplicată pe elementele structurii, printr-o construcție de forma:

minimumBy (comparing f) structura.

Exemple:

> maximalModularPartition <mulțimea partițiilor> diagram
fromList [fromList [1,2,3,4],fromList [5]]

> maximalModularPartition <mulțimea partițiilor> $ removeNode 5 diagram
fromList [fromList [1,2],fromList [3,4]]
-}
maximalModularPartition :: Ord a
                        => Set (Partition a)
                        -> Graph a
                        -> Partition a
maximalModularPartition partitions graph = -- toate partitiile 
  let var = Set.filter (\x -> if (isModularPartition x graph==True && Set.size x >1 ) -- pastrez toate partitiile modulare , fara sol banala , adica tot graful 
                              then True
                              else False) partitions
    in
      minimumBy (comparing Set.size) var -- alege elementul cu dimensiunea minima -> minimumBy (comparing f) structura

{-
Obține descompunerea modulară a unui graf. O puteți utiliza pentru
a experimenta manual cu maximalModularPartition.

Exemple:

>>> modularlyDecompose diagram                        
fromList [fromList [1,2,3,4],fromList [5]]

>>> modularlyDecompose $ removeNode 5 diagram
fromList [fromList [1,2],fromList [3,4]]
-}
modularlyDecompose :: Ord a
                   => Graph a
                   -> Partition a
modularlyDecompose graph = maximalModularPartition partitionSet graph
  where
    parts = partitions $ Set.toList $ nodes graph
    partitionSet = Set.fromList $ map toPartition parts

toPartition :: Ord a => [[a]] -> Partition a
toPartition = Set.fromList . map Set.fromList
