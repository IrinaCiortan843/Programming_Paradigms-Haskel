module AlgebraicGraph where

import Data.Set (Set)
import qualified Data.Set as Set

data AlgebraicGraph a
    = Empty
    | Node { label :: a }
    | Overlay { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    | Connect { left :: AlgebraicGraph a, right :: AlgebraicGraph a }
    deriving (Ord)

-- (1, 2), (1, 3)
-- 1 * (2 + 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
-- 1 * (2 * 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
1.as folosi constructor de tip g-> AlegbraicGraph / StandardGraph

class Graph g where
  empty :: g a
  node :: a -> g a
  inNeighbors :: Ord a => a -> g a -> Set a 
  outNeighbors :: Ord a => a -> g a -> Set a 

2.pt nodes :

nodes:: Ord a => g a -> Set a

-}

{-
*** TODO ***

Funcția nodes din etapa 2.
-}
nodes :: Ord a => AlgebraicGraph a -> Set a
nodes Empty = Set.empty
nodes (Node a)= Set.singleton a
nodes (Overlay left right)=Set.union (nodes left) (nodes right)
nodes (Connect left right)=Set.union (nodes left) (nodes right) 
{-
*** TODO ***

Funcția edges din etapa 2.
-}
edges :: Ord a => AlgebraicGraph a -> Set (a, a)
edges Empty = Set.empty
edges (Node a)= Set.empty 
edges (Overlay left right)=Set.union (edges left) (edges right)
edges (Connect left right)=Set.union 
                          (Set.union (edges left) (edges right)) 
                          (Set.cartesianProduct (nodes left) (nodes right))
{-
*** TODO ***

Funcția outNeighbors din etapa 2.
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
*** TODO ***

Funcția inNeighbors din etapa 2.
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
*** TODO 1 (10p) ***

Instanțiați clasa Num cu tipul (AlgebraicGraph a), astfel încât:

* un literal întreg să fie interpretat ca un singur nod cu eticheta egală
    cu acel literal
* operația de adunare să fie intepretată ca Overlay
* operația de înmulțire să fie interpretată drept Connect.

Celelalte funcții din clasă nu sunt relevante. Veți obține warning-uri
pentru neimplementarea lor, dar puteți să le ignorați.

După instanțiere, veți putea evalua în consolă expresii ca:

>>> 1 :: AlgebraicGraph Int
1

>>> 1*(2+3) :: AlgebraicGraph Int
(1*(2+3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger x = Node (fromInteger x)
    
    (+) = Overlay
    
    (*) = Connect

{-
*** TODO 2 (10p) ***

Instanțiați clasa Show cu tipul (AlgebraicGraph a), astfel încât reprezentarea 
sub formă de șir de caractere a unui graf să reflecte expresiile aritmetice 
definite mai sus. Puteți pune un nou rând de paranteze pentru fiecare 
subexpresie compusă.

Exemple:

>>> Node 1
1

>>> Connect (Node 1) (Overlay (Node 2) (Node 3))
(1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show graph = go graph
      where 
        go (Node x)= show x
        go (Overlay l r)= "("++ go l ++ "+" ++ go r ++")"
        go (Connect l r)= "("++ go l ++ "*" ++ go r ++")"

{-
*** TODO 3 (10p) ***

Observați că instanța predefinită de Eq pentru tipul (AlgebraicGraph a)
nu surprinde corect egalitatea a două grafuri, deoarece același graf
conceptual poate avea două descrieri simbolice diferite.

Prin urmare, instanțiați clasa Eq cu tipul (AlgebraicGraph a), astfel încât
să comparați propriu-zis mulțimile de noduri și de arce.

Exemple:

>>> Node 1 == 1
True

>>> Node 1 == 2
False

>>> angle == 1*2 + 1*3
True

>>> triangle == (1*2)*3
True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1==g2 = if (nodes g1==nodes g2 && edges g1==edges g2)
             then True 
             else False


{-
*** TODO 4 (15p) ***

Implementați funcția extend, care extinde un graf existent, atașând noi 
subgrafuri arbitrare în locul nodurilor individuale. Funcția primită ca prim 
parametru determină această corespondență între noduri și subgrafuri. Observați 
că tipul etichetelor noi (b) poate diferi de al etichetelor vechi (a).

Exemplu:

>>> extend (\n -> if n == 1 then 4+5 else Node n) $ 1*(2+3)
((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
extend f graph = go graph  -- inlocuieste graf cu un alt subgraf  ex anterior inlocuieste 1 cu 4+5 
  where 
    go Empty = Empty
    go (Node x)= f x
    go (Overlay l r)= Overlay (go l) (go r)
    go (Connect l r)= Connect (go l) (go r)

{-
*** TODO 5 (15p) ***

Implementați funcția splitNode, care divizează un nod în mai multe noduri,
cu eliminarea nodului inițial. Arcele în care era implicat vechiul nod trebuie 
să devină valabile pentru noile noduri.

CONSTRÂNGERI:

* Implementați splitNode folosind extend!

Exemple:

>>> splitNode 2 (Set.fromList [4,5]) triangle
(1*((4+(5+_))*3))
-}
splitNode :: Ord a
          => a                 -- nodul divizat
          -> Set a             -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets graph = -- inlocuieste node cu targets 
  let f a = if a==node 
                  then Set.foldl (\acc x -> Overlay acc (Node x) ) Empty targets -- transform targets din set in graf
                  else Node a
    in
    extend f graph -- parcurge tot graful si aplica functia pe fiecare nod 
{-
*** TODO 6 (5p) ***

Instanțiați clasa Functor cu constructorul de tip AlgebraicGraph, astfel încât 
să puteți aplica o funcție pe toate etichetele unui graf. fmap reprezintă 
generalizarea lui map pentru alte structuri.

CONSTRÂNGERI:

* Implementați fmap folosind extend!

Exemple:

>>> fmap (+ 10) $ 1*(2+3) :: AlgebraicGraph Int
(11*(12+13))
-}
instance Functor AlgebraicGraph where --aplic o functie pe fiecare nod problema e ce intoarce functia
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph =            -- f a->b
      let g x = Node (f x)    -- g a->graph b deoarece grpah(f: a->b)
        in 
        extend g graph 

{-
*** TODO 7 (10p) ***

Implementați funcția mergeNodes, care îmbină mai multe noduri într-unul singur, 
pe baza unei proprietăți respectate de nodurile îmbinate, cu eliminarea 
acestora. Arcele în care erau implicate vechile noduri vor referi nodul nou.

CONSTRÂNGERI:

* Implementați mergeNodes folosind fmap!

Exemple:

>>> mergeNodes odd 4 triangle
(4*(2*4))
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph =
      fmap (\a -> if (prop a ==True)  -- aplica functia pe toate nodurile grafului 
                  then node 
                  else a) graph
{-
*** TODO 8 (10p) ***

Implementați funcția filterGraph, care filtrează un graf, păstrând doar 
nodurile care satisfac proprietatea dată.

CONSTRÂNGERI:

* Implementați filterGraph folosind extend!

Exemplu:

>>> filterGraph odd triangle
(1*(_*3))
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = 
  let f a = if ( prop a == True )
            then Node a
            else Empty
     in 
      extend f graph 

{-
*** TODO 9 (5p) ***

Implementați funcția removeNode, care întoarce graful rezultat prin eliminarea 
unui nod și a arcelor în care acesta este implicat. Dacă nodul nu există, 
întoarce același graf.

CONSTRÂNGERI:

* Implementați removeNode folosind filterGraph!

Exemplu:

>>> removeNode 2 triangle
(1*(_*3))
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = 
  let prop a = if (a/=node)
               then True
               else False 
    in 
      filterGraph prop graph 
