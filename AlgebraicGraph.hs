module AlgebraicGraph where

import Data.Set (Set)
import qualified Data.Set as Set

{-

data StandardGraph a = StandardGraph
   (Set a )
   (Set( a , a ))

fromComponents n e=StandardGraph
  (Set.fromList n)
  (Set.fromList e)

-}
data AlgebraicGraph a
    = Empty
    | Node { label :: a }
    | Overlay { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    | Connect { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    deriving (Eq, Ord, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
*** TODO 1 (10p) ***

Implementați funcția nodes, care întoarce mulțimea nodurilor grafului.

Hint: Set.union

Exemple:

>>> nodes triangle
fromList [1,2,3]
-}
nodes :: Ord a => AlgebraicGraph a -> Set a
nodes Empty = Set.empty
nodes (Node a)= Set.singleton a
nodes (Overlay left right)=Set.union (nodes left) (nodes right)
nodes (Connect left right)=Set.union (nodes left) (nodes right) 

{-
*** TODO 2 (10p) ***

Implementați funcția edges, care întoarce mulțimea arcelor grafului.

Hint: Set.union, Set.cartesianProduct

Exemple:

>>> edges triangle
fromList [(1,2),(1,3),(2,3)]
-}
edges :: Ord a => AlgebraicGraph a -> Set (a, a)
edges Empty = Set.empty
edges (Node a)= Set.empty 
edges (Overlay left right)=Set.union (edges left) (edges right)
edges (Connect left right)=Set.union 
                          (Set.union (edges left) (edges right)) 
                          (Set.cartesianProduct (nodes left) (nodes right))

{-
*** TODO 3 (15p) ***

Implementați funcția outNeighbors, care întoarce mulțimea nodurilor înspre care 
pleacă arce dinspre un nod sursă.

CONSTRÂNGERI:

* NU folosiți funcția edges definită mai sus, pentru că ar genera prea multe 
  muchii inutile.
* Evitați trimiterea către apelurile recursive a parametrilor care nu se 
  modifică. De exemplu, parametrul node nu se modifică, în timp ce parametrul 
  graph se modifică. Pentru aceasta, definiți o funcție recursivă locală (de 
  exemplu, în where), care să primească drept parametri doar entitățile 
  variabile de la un apel recursiv la altul.

Exemple:

>>> outNeighbors 1 triangle
fromList [2,3]
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> Set a
outNeighbors node graph = go graph
  where
    go Empty=Set.empty
    go (Node a)= Set.empty
    go (Overlay left right)=Set.union 
                            (go left)
                            (go right)
    go (Connect left right)=Set.union -- each left->each right ! dar nu si invers 
                            (Set.union (go left) (go right))
                            rest
                            where 
                              rest= if Set.member node (nodes left) 
                                    then nodes right -- deci fie iau right
                                    else Set.empty   -- fie nu iau nimic pt ca muchiile sunt doar de la left la right nu si invers 

{-
*** TODO 4 (15p) ***

Implementați funcția inNeighbors, care întoarce mulțimea nodurilor dinspre care 
pleacă arce înspre un nod destinație.

CONSTRÂNGERI:

* NU folosiți funcția edges definită mai sus, pentru că ar genera prea multe 
  muchii inutile.
* Evitați trimiterea către apelurile recursive a parametrilor care nu se 
  modifică (vezi outNeighbors).

Exemple:

>>> inNeighbors 3 triangle
fromList [1,2]
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> Set a
inNeighbors node graph = go graph
  where
    go Empty=Set.empty
    go (Node a)= Set.empty
    go (Overlay left right)=Set.union 
                            (go left)
                            (go right)
    go (Connect left right)=Set.union -- each left->each right ! dar nu si invers 
                            (Set.union (go left) (go right))
                            rest
                            where 
                              rest= if Set.member node (nodes right) 
                                    then nodes left 
                                    else Set.empty 

{-
*** TODO 5 (15p) ***

Implementați funcția removeNode, care întoarce graful rezultat prin eliminarea 
unui nod și a arcelor în care acesta este implicat. Dacă nodul nu există, 
întoarce același graf.

CONSTRÂNGERI:

* Evitați trimiterea către apelurile recursive a parametrilor care nu se 
  modifică (vezi outNeighbors).

Exemple:

>>> removeNode 2 triangle
Connect {left = Node {label = 1}, right = Connect {left = Empty, right = Node {label = 3}}}

>>> nodes $ removeNode 2 triangle
fromList [1,3]

>>> edges $ removeNode 2 triangle
fromList [(1,3)]
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
--removeNode node graph = undefined
removeNode node graph = go graph
  where 
    go Empty=Empty 
    go (Node a)= if a==node 
                  then Empty 
                  else Node a 
    go (Overlay left right)= Overlay (go left) (go right)                
    go (Connect left right)= Connect (go left) (go right)

{-
*** TODO 6 (20p) ***

Implementați funcția splitNode, care divizează un nod în mai multe noduri,
cu eliminarea nodului inițial. Arcele în care era implicat vechiul nod trebuie 
să devină valabile pentru noile noduri.

CONSTRÂNGERI:

* Evitați trimiterea către apelurile recursive a parametrilor care nu se 
  modifică (vezi outNeighbors).

Exemple:

>>> splitNode 2 (Set.fromList [4,5]) triangle
Connect {left = Node {label = 1}, right = Connect {left = Overlay {left = Node {label = 4}, right = Overlay {left = Node {label = 5}, right = Empty}}, right = Node {label = 3}}}

>>> nodes $ splitNode 2 (Set.fromList [4,5]) triangle
fromList [1,3,4,5]

>>> edges $ splitNode 2 (Set.fromList [4,5]) triangle
fromList [(1,3),(1,4),(1,5),(4,3),(5,3)]
-}
splitNode :: Ord a
          => a                 -- nodul divizat
          -> Set a             -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets graph = go graph 
  where 
    go Empty=Empty 
    go (Node a)= if a==node 
                  then Set.foldl (\acc x -> Overlay acc (Node x) ) Empty targets -- transform targets din set in graf
                  else Node a
    go (Overlay left right)= Overlay (go left) (go right)                
    go (Connect left right)= Connect (go left) (go right)
{-
*** TODO 7 (15p) ***

Implementați funcția mergeNodes, care îmbină mai multe noduri într-unul singur, 
pe baza unei proprietăți respectate de nodurile îmbinate, cu eliminarea 
acestora. Arcele în care erau implicate vechile noduri vor referi nodul nou.

CONSTRÂNGERI:

* Evitați trimiterea către apelurile recursive a parametrilor care nu se 
  modifică (vezi outNeighbors).

Exemple:

>>> mergeNodes odd 4 triangle
Connect {left = Node {label = 4}, right = Connect {left = Node {label = 2}, right = Node {label = 4}}}

>>> nodes $ mergeNodes odd 4 triangle
fromList [2,4]

>>> edges $ mergeNodes odd 4 triangle
fromList [(2,4),(4,2),(4,4)]
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = go graph 
  where 
    go Empty=Empty 
    go (Node a)= if (prop a==True ) -- daca e unul din nodurile ce trebuie removed
                  then Node node 
                  else Node a
    go (Overlay left right)= Overlay (go left) (go right)                
    go (Connect left right)= Connect (go left) (go right)