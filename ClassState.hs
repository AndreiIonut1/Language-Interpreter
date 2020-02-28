module ClassState
where

import Data.Maybe    
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

--ClassState-ul este o pereche: un map cu 2 key: Var si Func + un String
--Stringul reprezinta clasa parinte.
--Fiecarei intrari(Var & Func) ii este asociat un alt map de la String la [[String]]
--De exemplu, pentru o functie mapul va folosi drept cheie numele functiei si drept
--valoare lista de argumente a functiei.
--Folosim o lista de liste de String pentru a putea retine si functii supraincarcate,
--caz in care un nume de functie are asociate mai multe liste de argumente
type ClassState = (Map.Map InstrType (Map.Map String [[String]]), String)

--Construim mapul cu 2 intrari: Func si Var, fiecare cheie are la randul ei asociata
--drept valoare un map gol.
--ClassState = ((Var, mapForVars) (Func, mapForFuncs)) (parentClass)
initEmptyClass :: ClassState
initEmptyClass = (Map.insert Func Map.empty (Map.insert Var Map.empty Map.empty), "")

--Insereaza o noua variabila/functie in containerul de clasa
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (classState, parent) instrType list =  
        (Map.insert instrType (Map.insert key value myMap) classState, parent)
        where
            --myMap este ori mapul asociat variabilelor ori mapul asociat 
            --functiilor in functie de instrType
            --stim sigur ca exista deci Map.lookup nu poate sa intoarca Nothing, 
            --deci putem sa folosim direct fromJust
            myMap = fromJust $ (Map.lookup instrType classState)
            --cheie este numele variabilei/functiei
            key   = head list 
            --verificam daca exista deja o variabila/functie cu acelasi nume
            value = if Map.member key myMap
                     --vrem sa adaugam o functie supraincarcata, ii adaugam
                     --lista de argumente la lista existenta 
                     then ((fromJust $ Map.lookup key myMap) ++ [tail list])
                     --nu exista, deci trebuie creata o intrare in map.
                     --intoarce o lista de lista pentru ca in viitor putem sa
                     --avem mai multe liste de argumente pentru aceeasi cheie
                     else [tail list]

--Seteaza parintele unei clase
setParent :: ClassState -> String -> ClassState
setParent (classState, p) parent = (classState, parent)

--Intoarce toate functiile/variabilele stocate intr-un container de clasa
getValues :: ClassState -> InstrType -> [[String]]
getValues (classState, parent) instrType = Map.foldrWithKey buildList [] myMap
        where
            myMap = fromJust $ Map.lookup instrType classState
            --k este cheia(numele functiei/variabilei)
            --v este valoarea(lista de liste de argumente pentru eventualele functii supraincarcate)
            --mapul adauga la inceputul fiecarei liste de argumente numele functiei
            --apoi rezultatul este concatenat la acumulator pentru a obtine lista finala
            buildList k v acc = (map (\e -> k : e) v) ++ acc

--Primeste un nume de variabila si un classtate(Global) si intoarce clasa
--de care apartine variabila           
getVarClass :: String -> ClassState -> Maybe String
getVarClass varName (classState, parent) = 
        --verificam daca variabila exista in clasa
        case(Map.lookup varName myMap) of 
            --deoarece functiile sunt reprezentate ca liste de liste si variabilele
            --vor fi reprezentate la fel, chiar daca ele au asociat un singur
            --string(clasa din care fac parte)
            --vom obtine un rezultat de forma [[varClass]], din care extragem
            --sirul luand de 2 ori primul element
            Just varClass -> Just ((varClass !! 0) !! 0)
            --variabila nu exista
            Nothing -> Nothing 
        where
            --mapul pentru variabile al clasei
             myMap = fromJust $ Map.lookup Var classState                                  

