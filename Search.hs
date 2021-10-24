{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state :: s, -- starea
    action :: Maybe a, --actiunea care l-a dus in starea respectiva
    parent :: Node s a, -- parintele
    level :: Int, -- adancimea
    heuristic :: Float, -- fct euristica
    children :: [Node s a] -- vector de noduri copil
} | Void -- void folosesc atunci cand n-am parinte

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    n1 == n2 = state n1 == state n2

instance Ord s => Ord (Node s a) where
    n1 <= n2 = state n1 <= state n2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = \node -> checkParent $ parent node
    where
        checkParent :: Node s a -> Maybe (Node s a) 
        checkParent Void = Nothing
        checkParent nod = Just nod

nodeDepth :: Node s a -> Int
nodeDepth = \node -> level node

nodeChildren :: Node s a -> [Node s a]
nodeChildren = \node -> children node

nodeHeuristic :: Node s a -> Float
nodeHeuristic = \node -> heuristic node

nodeAction :: Node s a -> Maybe a
nodeAction = \node -> action node

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

--Creez nodul radacina apoi recursiv construiesc si descendentii
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = this {children = map (\succ -> createDescendants (fst succ) (snd succ) this) (successors initialState) }
    where this = Node initialState Nothing Void 0 (h initialState) [] -- nodul radacina construit

createDescendants :: (ProblemState s a, Eq s) => a -> s -> Node s a -> Node s a 
createDescendants act presentState parent = this {children = map (\succ -> createDescendants (fst succ) (snd succ) this) (successors presentState)}
    where this = Node presentState (Just act) parent ((level parent) + 1) (h presentState) [] --copilul curent

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

--elimin din lista de copii nodurile care se afla in multimea de vizitati
suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\crtnode -> if S.member (state crtnode) visited then False else True) (children node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

--inserez nodul, cu cheia fiind suma dintre euristica si adancime(adica suma dintre distanta
--de la sursa si distanta pana la destinatie). Folosesc functia data ca prim parametru lui insertWith
--pt a realiza combinarea cheilor in cazul in care sunt mai multe(adica o aleg pe cea minima)
insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = PQ.insertWith min node
    ((fromIntegral (level node)) + heuristic node) frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

--Insereaza toti succesorii, folosint un fold cu care ii "parcurg" pe toti si ii inserez
--pe rand cu insertWith
insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = foldl (\pq val -> PQ.insertWith min val
    ((fromIntegral (level val)) + heuristic val) pq) frontier (suitableSuccs node visited)

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

--aici prima data extrag un nod din frontiera, il pun in multimea de vizitati si actualizez frontiera.
--apoi apelez fct astar_aux care introduce succesorii in frontiera daca e necesar si reapeleaza iar
--astar' pe noua coada. Daca am ajuns la configuratie finala, astar_aux intoarce nodul final
astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = astar_aux (fst (deleteFindMin frontier)) (S.insert (state (fst (deleteFindMin frontier))) visited) (snd (deleteFindMin frontier))
    where
        astar_aux ::(ProblemState s a, Ord s) => Node s a -> (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
        astar_aux node visited frontier
            | isGoal $ state node = node
            | otherwise = astar' visited (insertSuccs node frontier visited)
{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

--Param initali ai algoritmului astar sunt multimea vida pt vizitati si o coada de prioritate in care
--am doar nodul radacina
astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = astar' S.empty (PQ.insertWith min initialNode ((heuristic initialNode) + (fromIntegral (level initialNode))) PQ.empty)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

--Functie care intoarce parintele nodului curent, daca exista.
--Daca nu intoarce void
generate_parent :: Node s a -> Node s a
generate_parent Void = Void
generate_parent nod = parent nod

--Ma folosesc de lazy evaluation si de functia iterate care imi aplica functia de generate_parent
--la infinit. Apoi, din parintii generati, extrag doar un numar egal cu adancimea, deoarece doar
--atatia sunt valizi, adica diferiti de Void. Inversez pentru a avea ordinea corecta.
extractPath :: Node s a -> [(a, s)]
extractPath goalNode = reverse $ map (\nod -> (fromJust (action nod), state nod)) $ take (level goalNode) (iterate (generate_parent) goalNode)