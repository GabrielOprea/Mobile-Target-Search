{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    --dimensiunea tablei de joc (linii si coloane)
    dim :: (Int, Int),
    --Pozitia si caracterul pentru hunter
    hunt :: (Position, Char),
    --Pozitiile targeturilor
    targets :: ([Target], Char),
    --Pozitiile obstacolelor
    obst :: ([Position], Char),
    --Lista de perechi pentru pozitiile gateway-urilor/portalelor
    portals :: ([(Position, Position)], Char)
} deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

-- Transforma o lista de perechi intr-o lista de dimensiune dubla
my_unzip :: [(a, a)] -> [a]
my_unzip l = concat (map my_unzip_aux l) where
    -- construieste o lista de 2 elemente dintr-o pereche
    my_unzip_aux :: (a, a) -> [a]
    my_unzip_aux p = [fst p, snd p]


-- transforma un joc intr-un string tiparibil pe ecran
gameAsString :: Game -> String
-- obtine fiecare caracter asociat cu pozitiile generate
gameAsString game = init $ map get_char generate_pos
    where
        --generez toate pozitiile posibile in matricea de joc, cu coordonata Ox
        --de la 0..nr de linii si Oy de la 0 la nr de coloane
        generate_pos :: [Position]
        generate_pos = [(x, y) | x <- [0..(fst $ dim game)], y <- ([0..(snd $ dim game)] ++ [-1]) ]

        --obtin caracterul asociat entitatii din joc aflate pe pozitia respectiva
        get_char :: Position -> Char
        get_char elem_pos
            | snd elem_pos == -1 = '\n'  -- sfarsit de linie
            | any (==elem_pos) (map position $ fst $ targets game) = snd $ targets game -- avem target pe pozitia elem_pos
            | any (==elem_pos) (fst $ obst game) = snd $ obst game -- obstacole pe pozitia elem_pos
            | any (==elem_pos) (my_unzip $ fst $ portals game) = snd $ portals game -- avem un portal pe pozitia elem_pos
            | any (==elem_pos) [fst $ hunt game] = snd $ hunt game -- avem hunterul pe pozitia elem_pos
            | otherwise = ' ' -- pozitia este una libera

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
-- initializez o instanta a lui Game cu dimensiunea corecta, cu hunterul pe pozita (1,1),
-- cu listele de targeturi si portale(gateways) vide. Exceptie face lista de obstacole
emptyGame lins cols = Game (lins - 1, cols - 1) ((1,1), '!') ([], '*') generate_obst ([], '#')
    where
        generate_obst :: ([Position], Char)
        -- generez lista de obstacole, filtrand doar pozitiile ce sunt la marginile matricei,
        -- adica au Ox = 0 sau nr de linii, Oy = 0 sau nr de coloane
        generate_obst = (filter (\(a, b) -> a == 0 || b == 0 || a == (lins -1) || b == (cols - 1))
                             [(x, y) | x <- [0..(lins - 1)], y <- [0..(cols - 1)]], '@')

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

--Verifica daca o pozitie primita este invalida in jocul curent
--Este invalida daca este asociata cu un obstacol, sau daca depaseste una din
--dimensiunile tabelei
invalid :: Position -> Game -> Bool
invalid new_pos game = (any (==new_pos) (fst $ obst game))
    || (fst new_pos < 0) || (fst new_pos > (fst $ dim game))
    || (snd new_pos < 0)  || (snd new_pos > (snd $ dim game))

addHunter :: Position -> Game -> Game
addHunter new_pos game
    | invalid new_pos game = game -- intorc acelasi joc
    | otherwise = game {hunt = (new_pos, '!')} --intorc acelasi joc, dar cu atributul hunter modificat
{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}

--extrag targetii, si adaug la inceput in lista respectiva cu cons targetul nou creat din
--behavior si pozitie
addTarget :: Behavior -> Position -> Game -> Game
addTarget bh new_pos game = game {targets =  (((Target new_pos bh):(fst $ targets game)), '*')}

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

--adaug perechea primita in lista de perechi de portale
addGateway :: (Position, Position) -> Game -> Game
addGateway pair game = game {portals =  ((pair:(fst $ portals game)), '#')}
{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

--adaug obstacolul dat de pozitie in lista aferenta
addObstacle :: Position -> Game -> Game
addObstacle new_pos game = game {obst = ((new_pos:(fst $ obst game)), '@')}

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

--Incearca sa faca o mutare
attemptMove :: Position -> Game -> Maybe Position
attemptMove new_pos game
    | any (==new_pos) (fst $ obst game) = Nothing --daca vrea sa se duca intr-un obstacol, intoarce nothing
    | any (==new_pos) (my_unzip $ fst $ portals game) = Just $ teleport new_pos game --daca vrea sa se duca intr-un portal, teleporteaza-l
    | otherwise = Just new_pos --daca pozitia e libera, atunci se duce in pozitia respectiva

--Teleporteaza un target prin portalul dat de pozitia new_pos la portalul sau pereche
teleport :: Position -> Game -> Position
--se cauta portalul destinatie
teleport new_pos game = if fst match == new_pos then snd match else fst match 
    where 
        --obtine perechea de portale prin care se trece
        match = last $ filter (\(a, b) -> a == new_pos || b == new_pos) (fst $ portals game)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

--Functie auxiliara care primeste pozitia initiala a targetului, pozitia pe care vrea sa se mute,
--jocul in care se afla si targetul efectiv si intoarce acel target, dar cu pozitia actualizata
moveTarget :: Position -> Position -> Game -> Target -> Target
moveTarget old new game target
    --Daca pozitia finala nu e valida, incerc sa vad daca pe pozitia curenta am un portal(caz in care
    -- pot sa ma duc prin portal la pozitia finala.) Daca nu am portal, atunci raman pe loc
    | invalid new game = if any (==old) (my_unzip $ fst $ portals game) then target {position = teleport old game,
                                                                                     behavior = behavior target} else target
    --Daca pe pozitia finala am portal, trec prin el
    | any (==new) (my_unzip $ fst $ portals game) = target {position = teleport new game,
                                                            behavior = behavior target}
    --Daca pozitia finala e libera, ma mut pe ea
    | otherwise = target {position = new,
                            behavior = behavior target}

--Analog cu moveTarget, dar pentru hunter
moveHunter :: Position -> Position -> Game -> Position
moveHunter old new game
    | invalid new game = if any (==old) (my_unzip $ fst $ portals game) then teleport old game else old
    | any (==new) (my_unzip $ fst $ portals game) = teleport new game
    | otherwise = new

--Analog cu moveTarget, doar ca nu mai tin cont de portale
moveTargetWithoutGateway :: Position -> Game -> Target -> Target
moveTargetWithoutGateway new game target
    | invalid new game = target
    | otherwise = target {position = new,
                            behavior = behavior target}


--Functie care incearca sa mute target-ul la est cu moveHunter. Daca pozitia e invalida ramane pe
--loc; daca are portal, va trece prin acesta
goEast :: Behavior
goEast old game = moveTarget old (fst old, snd old + 1) game (last $ filter (\t -> position t ==old) (fst $ targets game)) 
{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest old game = moveTarget old (fst old, snd old - 1) game (last $ filter (\t -> position t ==old) (fst $ targets game)) 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth old game = moveTarget old (fst old - 1, snd old) game (last $ filter (\t -> position t ==old) (fst $ targets game)) 

--Functiile tryNorth si trySouth fac acelasi lucru ca cele cu goSouth/goNorth, doar ca nu se mai tine cont de gateways.
--Fac asta deoarece in bounce fac deja o verificare a acestui lucru, si nu am nevoie si ca functiile de mutare
--sa faca verificarea

tryNorth :: Behavior
tryNorth old game = moveTargetWithoutGateway (fst old - 1, snd old) game (last $ filter (\t -> position t ==old) (fst $ targets game)) 

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth old game = moveTarget old (fst old + 1, snd old) game (last $ filter (\t -> position t ==old) (fst $ targets game)) 

trySouth :: Behavior
trySouth old game = moveTargetWithoutGateway (fst old + 1, snd old) game (last $ filter (\t -> position t ==old) (fst $ targets game))

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce movement target_pos game
    -- daca pozitia finala coincide cu cea initiala, inseamna ca targetul e blocat, deci trebuie sa
    -- bounce-uiasca inapoi. In caz contrar, il mut la sud sau la nord in functie de parametrul int
    | movement == 1 = if (position $ trySouth target_pos game) == target_pos then (bounce (-1) target_pos game)
                        else (goSouth target_pos game) {behavior = (bounce 1)}
    | movement == (-1) = if (position $ tryNorth target_pos game) == target_pos then (bounce 1 target_pos game)
                        else (goNorth target_pos game) {behavior = (bounce (-1))}
    | otherwise = undefined

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
--Mapeaza functia data de behavior pt toti targetii
moveTargets :: Game -> Game
moveTargets game = game {targets = ((map (\target -> (behavior target (position target) game)) (fst $ targets game)), '*') }

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
-- Primeste o pozitie a hunterului si un target si verifica pe rand toti cei 4 vecini.
-- Daca hunterul nu e pe pozitie adiacenta cu targetul intoarce false
isTargetKilled :: Position -> Target -> Bool
isTargetKilled hunt_pos target
    | (fst hunt_pos == (fst (position target)) + 1) && (snd hunt_pos == (snd (position target))) = True
    | (snd hunt_pos == (snd (position target)) + 1) && (fst hunt_pos == (fst (position target))) = True
    | (fst hunt_pos == (fst (position target)) - 1) && (snd hunt_pos == (snd (position target))) = True
    | (snd hunt_pos == (snd(position target)) - 1) && (fst hunt_pos == (fst (position target)))  = True
    | otherwise = False

--Inversarea functiei isTargetKilled
notKilled :: Position -> Target -> Bool
notKilled hunt_pos target = not $ isTargetKilled hunt_pos target

{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
-- Intoarce o pozitie adiacenta, in functie de directie
moveInDirection :: Direction -> Position -> Position
moveInDirection North = \(x, y) -> (x - 1, y)
moveInDirection South = \(x, y) -> (x + 1, y)
moveInDirection East = \(x, y) -> (x, y + 1)
moveInDirection West = \(x, y) -> (x, y - 1)

--Realizez pe rand pasii descrisi in TODO-ul de mai sus. La primul pas, deplasez hunterul.
--Pentru aceasta am definit anterior functia moveHunter, iar pozitia finala este data
--de pozitia adiacenta descrisa de moveInDirection.
first_step :: Direction -> Game -> Game
first_step direction game = game {hunt = ((moveHunter (fst $ hunt game) (moveInDirection direction (fst $ hunt game)) game), '!')}

--Pentru pasul al doilea, folosesc un filter pentru a "filtra" toti targetii care sunt in viata, adica care nu
--se invecineaza cu hunterul mutat
second_step :: Direction -> Game -> Game
second_step direction game = (first_step direction game)
    {targets = ((filter (notKilled (fst $ hunt (first_step direction game))) (fst $ targets (first_step direction game))), '*')}

-- Apoi, mut iar targetii, cu functia moveTargets
third_step :: Direction -> Game -> Game
third_step direction game = moveTargets (second_step direction game)

--Fac acelasi lucru ca la step 2, doar ca pentru jocul actualizat de la third_step
fourth_step :: Direction -> Game -> Game
fourth_step direction game = (third_step direction game) 
    {targets = ((filter (notKilled (fst $ hunt (third_step direction game))) (fst $ targets (third_step direction game))), '*')}

--In functie de parametrul bool, fac fie 1 pas, fie 4
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction elim game
    | not elim = first_step direction game
    | otherwise = fourth_step direction game

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not ((length $ fst $ targets game) == 0)


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined

generate_adj_targets :: Game -> ([Target], Char)
generate_adj_targets game = ((filter (\target -> isTargetKilled (fst $ hunt game) target) (fst $ targets game)),'*')

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors = \game -> (North, advanceGameState North False game):(South, advanceGameState South False game)
                            :(West, advanceGameState West False game):(East, advanceGameState East False game):[]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}

    isGoal = \game -> (length $ fst $ generate_adj_targets game) /= 0

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h = \game -> if (length $ fst $ targets game) == 0 then 0.0 else 
                     if (length $ fst $ generate_adj_targets game) == 0 then hEuclidean (position $ head $ fst $ targets game) (fst $ hunt game)
                        else hEuclidean (position $ head $ fst $ generate_adj_targets game) (fst $ hunt game)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
