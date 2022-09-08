module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes graph = case graph of
    Empty ->
        S.empty
    Node x ->
        S.fromList [x]
    Overlay g1 g2 ->
        S.union (nodes g1) (nodes g2)
    Connect g1 g2 ->
        S.union (nodes g1) (nodes g2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges graph = case graph of
    Empty ->
        S.empty
    Node x ->
        S.empty
    Overlay g1 g2 ->
        S.union (edges g1) (edges g2)
    Connect g1 g2 ->
        S.union (S.cartesianProduct (nodes g1) (nodes g2)) (S.union (edges g1) (edges g2))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node graph = case graph of
    Node x ->
        S.empty
    Overlay g1 g2 ->
        S.union (outNeighbors node g1) (outNeighbors node g2)
    Connect g1 g2 ->
        if elem node $ nodes g1 then
            nodes g2
        else
            S.empty


{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node graph = case graph of
    Node x ->
        S.empty
    Overlay g1 g2 ->
        S.union (inNeighbors node g1) (inNeighbors node g2)
    Connect g1 g2 ->
        if elem node $ nodes g2 then
            S.union (nodes g1) (inNeighbors node g2)
        else
            S.empty
{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = f graph
                        where f graph = case graph of
                                            Node x -> if x /= node then
                                                          Node x
                                                      else 
                                                          Empty
                                            Overlay g1 g2 ->
                                                Overlay (f g1) (f g2)
                                            Connect g1 g2 ->
                                                Connect (f g1) (f g2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = f news graph 
                            where f news graph = case graph of
                                        Node x -> if x == old then
                                                    if null news /= True then
                                                        Overlay (Node (head news)) (f (tail news) graph)
                                                    else
                                                        Empty
                                                  else
                                                     Node x
                                        Overlay g1 g2 -> 
                                            Overlay (f news g1) (f news g2)
                                        Connect g1 g2 ->
                                            Connect (f news g1) (f news g2)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = f [node] graph
                              where f node graph = case graph of
                                            Node x ->  if prop x then
                                                          if null node /= True then
                                                              Overlay (Node (head node)) (f (tail node) graph)
                                                          else
                                                              Empty
                                                       else
                                                          Node x
                                            Overlay g1 g2 ->
                                                Overlay (f node g1) (f node g2)
                                            Connect g1 g2 -> 
                                                Connect (f node g1) (f node g2)