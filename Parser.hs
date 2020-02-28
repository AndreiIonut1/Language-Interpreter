module Parser
where

import Util
import InferenceDataType
import ClassState
import TextProcessing
import Data.Maybe
import qualified Data.Map as Map

--Un program este un map de la numele clasei la containerul de clasa
type Program = Map.Map String ClassState

--Intoarce un program gol
--Un program gol va contine clasa Global cu parintele Global.
--Initial containerul pentru Global este si el gol.
initEmptyProgram :: Program
initEmptyProgram = Map.insert "Global" (setParent initEmptyClass "Global") Map.empty

--Intoarce toate variabilele din program. 
--Toate variabilele sunt stocate in clasa Global.
--"Global" exista mereu, deci Map.lookup nu va intoarce Nothing
getVars :: Program -> [[String]]
getVars program = getValues (fromJust $ Map.lookup "Global" program) Var
            
--Intoarce toate clasele din program
getClasses :: Program -> [String]
getClasses program = Map.keys program -- clasele sunt cheile

--Intoarce clasa parinte a unei clase din program
--Determinam containerul de clasa asociat cu numele "className"
--si intoarcem al doilea element al perechii.
--In cazul in care clasa nu exista, intoarcem sirul vid
--Probabil ar fi fost mai potrivit sa folosim Maybe String, dar
--nu ar respecta prototipul dat in cerinta.
getParentClass :: String -> Program -> String
getParentClass className program = case(Map.lookup className program) of
                                        Just myMap -> snd myMap
                                        Nothing -> ""

--Intoarce toate functiile unei clase din program
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass className program = case (Map.lookup className program) of 
                                        Just myMap -> getValues myMap Func
                                        Nothing -> [] --clasa nu exista, intoarcem lista vida

{-
    NewClass  numeClasa parinteClasa
    NewVar    numeVariabila numeClasa
    NewFunc   tipReturnat simbolClasa simbolFunctie listaParam
    Infer     numeVariabila expresie
    NoInstr   permite tratarea usoara a cazurilor de instruciuni invalide/linii goale
-}
data Instruction = NoInstr | NewClass String String | NewVar String String | 
                   NewFunc String String String [String] | Infer String Expr deriving Show

 
--Parcurgem inputul si genereaza o lista de instructiuni
--strtok si trim vor genera instructiunile ca sir iar
--stringToInstruction va genera pe baza sirurilor valori de tip Instruction
parse :: String -> [Instruction]
parse inputString = map (stringToInstruction) (map (trim) (strtok inputString '\n'))
                   
--Transforma un sir in instructiunea echivalenta
--De exemplu, "class Int extends Global" devine NewClass "Int" "Global"
stringToInstruction :: String -> Instruction                                   
stringToInstruction inputString
        | not (null argumentsList) = case (head argumentsList) of --determina tipul instructiunii
                                        "class" -> buildClassInstruction inputString
                                        "newvar" -> buildVarInstruction inputString
                                        "infer" -> buildInferInstruction inputString
                                        _ -> buildFuncInstruction inputString
        | otherwise = NoInstr  -- linie goala 
        where
            --imparte sirul instructiunii in tokenuri
            --de exemplu "class Int" devine ["class", "Int"]
            argumentsList = strtok inputString ' '                      

--Construieste instructiunea pentru a crea o noua clasa in program
buildClassInstruction :: String -> Instruction
buildClassInstruction inputString = NewClass className classParent
        where
            argumentsList = strtok inputString ' '
            -- numele clasei este al doilea element din lista
            className     = argumentsList !! 1
            classParent   = if (length argumentsList > 2) -- extends apare explicit
                                then (argumentsList !! 3) --clasa parinte
                                else "Global" --parintele default este "Global"

--Construieste instructiunea pentru a crea o noua variabila in program
buildVarInstruction :: String -> Instruction                                 
buildVarInstruction inputString = NewVar varName varType
        where
            --imparte in "newvar VarName" si "varType"
            argumentsList = map (trim) (strtok inputString '=')
            --tipul este al doilea element din lista
            varType       = argumentsList !! 1 
            --imparte "newvar VarName" in ["newvar", "VarName"] si ia numele variabilei
            varName       = (strtok (argumentsList !! 0) ' ') !! 1

--Construieste instructiunea pentru a crea o noua functie in program
buildFuncInstruction :: String -> Instruction
buildFuncInstruction inputString = if length headerList /= 3
                                        --functia a primit un numar invalid de argumente
                                        then 
                                            NoInstr
                                        else 
                                            NewFunc (headerList !! 0) (headerList !! 1) 
                                                    (headerList !! 2) argumentsList
                                    where
                                        --headerul functiei: return clasa nume
                                        headerList    = buildFunctionHeader inputString
                                        --argumentele functiei
                                        argumentsList = buildArgumentsList inputString      

--Intoarce lista de argumente a functiei ca String
buildArgumentsList :: String -> [String]
buildArgumentsList inputString = map trim (strtok parametersTemp2 ',') --delimiteaza argumentele
        where 
            --extrage tot ce se afla intre ()
            parametersTemp1 = drop (findIndex inputString '(' + 1) inputString
            parametersTemp2 = take ((length parametersTemp1) - 1) parametersTemp1

--Intoarce headerul functiei ca lista de String
buildFunctionHeader :: String -> [String]
buildFunctionHeader inputString = strtok (trim tempHeader2) ' '
        where
            --extrage tot ce se afla inainte de ()
            tempHeader1 = take (findIndex inputString '(') inputString
            --inlocuieste :: cu spatiu pentru a face parsarea usoara
            tempHeader2 = replaceSemicolons tempHeader1

--Construieste instructiunea pentru inferenta de tip pentru a determina tipul variabilei
--Va converti expresia de la String la FCall
buildInferInstruction :: String -> Instruction
buildInferInstruction inputString = Infer newVarName (buildFCall (expression !! 1))
        where
            --expression reprezinta expresia ce trebuie evaluata
            expression = strtok inputString '='
            newVarName  = trim $ drop (length "infer" + 1) (expression !! 0)


--Converteste o expresie de la String la FCall in mod recursiv
buildFCall :: String -> Expr
buildFCall inputString 
        --daca avem '.' este un apel de functiei
        | contains inputString '.' =  FCall callerVarName functionName rest
        --altfel avem o variabila(caz de baza)
        | otherwise = Va inputString
        where 
            argsTemp1 = drop (findIndex inputString '(' + 1) inputString
            argsTemp2 = take ((length argsTemp1) - 1) argsTemp1
            --reprezinta argumentele functiei evaluate
            arguments = map trim (strtokExtended argsTemp2 ',')
            --header reprezinta var.func
            header = take (findIndex inputString '(') inputString
            callPattern   = map trim (strtok (header) '.')
            --numele variabilei pe care o folosim pentru apel
            callerVarName = callPattern !! 0
            --numele functiei apelate
            functionName  = callPattern !! 1
            --argumentele apelului curent
            --si ele trebuie convertite pe rand la FCall
            rest = (map (buildFCall) arguments)

--Interpreteaza o instructiune si o adauga in program
interpret :: Instruction -> Program -> Program
interpret instruction program = 
        case instruction of
            --instructiunea pentru a adauga o clasa nou
            (NewClass  className classParent) -> addNewClass className classParent program
            --instructiunea pentru a adauga o variabila noua
            (NewVar varName className) -> addNewVar varName className program
            --instructiunea pentru a adauga o functie noua
            (NewFunc returnType className functionName argumentsList) -> 
                (addNewFunc returnType className functionName argumentsList program)
            --instructiunea pentru a adauga o noua variabila al carei tip trebuie dedus
            (Infer varName expr) -> 
                --expr reprezinta expresia ce trebuie evaluata
                --vom apela infer pe expresia curenta pentru a vedea
                --daca o putem evalua cu succes
                case(infer expr program) of 
                    --expresia se evalueaza in mod corespunzator
                    --construim pur si simplu o noua instructiune de tipul newVar varName = dataType
                    --parse va lua stringul si il va transforma intr-o instructiune si apelam
                    --interpret pentru a adauga efectiv instructiuna in program
                    Just dataType -> interpret ((parse ("newvar " ++ varName ++ " = " ++ dataType)) !! 0) program
                    --expresia nu are un tip valid, nu facem nicio modificare
                    Nothing -> program
            --nu avem nimic de executat                                                       
            NoInstr -> program

--Adauga o noua clasa in program
addNewClass :: String -> String -> Program -> Program
addNewClass className classParent program
        --clasa nu exista in program, o adaugam
        | Map.member className program == False = 
            if Map.member classParent program
                --clasa parinte exista
                --construim un nou container si il asociem numelui clasei
                then Map.insert className (setParent initEmptyClass classParent) program
                --clasa parinte nu exista, parintele va fi "Global"
                else Map.insert className (setParent initEmptyClass "Global") program
        --clasa exista deja, nu facem nimic
        | otherwise = program

--Adauga o noua variabila in program
addNewVar :: String -> String -> Program -> Program
addNewVar varName className program
        --clasa din care face parte variabila exista
        | Map.member className program == True =  
            --o inseram in clasa Global
            Map.insert "Global" (insertIntoClass globalClassState Var [varName, className]) program
        --incercam sa adaugam o variabila cu un tip necunoscut, nu se poate
        | otherwise = program
        where 
            --clasa global exista mereu
            globalClassState = fromJust $ Map.lookup "Global" program

--Adauga o noua functie in program
addNewFunc :: String -> String -> String -> [String] -> Program -> Program
addNewFunc returnType className functionName argumentsList program = 
        case(Map.lookup className program) of
            --clasa in care vrem sa adaugam functia exista
            --verificam daca argumentele si tipul returnat sunt tipuri cunoscute
            Just myMap -> if (validateFuncArgs argumentsList program) && (validateRet returnType program)
                            --inseram functia in program
                            then Map.insert className (insertIntoClass (myMap) Func arguments) program
                            --avem tipuri necunoscute, nu putem insera
                            else program
            --incercam sa adaugam o functie intr-o clasa care nu exista
            Nothing -> program  
        where
            arguments = if (argumentsList /= [[]]) 
                            --functia are argumente, adauga numele si tipul returnat
                            then ([functionName, returnType] ++ argumentsList)
                            --functia nu ia argumente, are doar un nume si tip returnat
                            else  ([functionName, returnType])   

--Verifica daca toate argumentele unei functii sunt de tip cunoscut
validateFuncArgs :: [String] -> Program -> Bool
validateFuncArgs argumentsList program 
        --verificam fiecare argument in parte folosind all
        --all intoarce true daca toate elementele listei verifica predicatul
        | (argumentsList /= [""]) == True = all (\arg -> validateRet arg program) argumentsList
        --nu avem argumente, ceea ce este valid
        | otherwise = True
    
--Verifica daca tipul returnat(actually any string) este de tip cunoscut
--Tipul este valid daca exista o clasa cu numele tipului
validateRet :: String -> Program -> Bool
validateRet returnType program = Map.member returnType program

--Evalueaza o expresie si ii determina tipul(daca este posibil)
infer :: Expr -> Program -> Maybe String
--Pentru o variabila tipul este stocat ca valoarea asociata numelui in clasa Global
infer (Va varName) program = getVarClass varName (fromJust $ Map.lookup "Global" program)

--Determina tipul unui apel de functie
infer (FCall varName functionName arguments) program = 
        --determina tipul variabilei din care se face apelul
        case (infer (Va varName) program) of
            Just varClass -> case (inferResult) of
                                --verifica daca exista functia apelata
                                --urcand pe lantul mostenirii in cazul in care nu
                                --exista in clasa curenta
                                Just inferArgs -> checkAllBaseClasses varClass functionName inferArgs program
                                Nothing -> Nothing
            --variabila nu exista
            Nothing -> Nothing
        where
            --rezultatul inferentei de tip pentru fiecare argument al apelului
            tempResult  = map (\expr -> infer expr program) arguments
            --daca macar unul dintre rezultate este Nothin inseamna ca apelul
            --este invalid si nu mai putem continua
            inferResult = if (all (\x -> x /= Nothing) tempResult) 
                                --converteste fiecare element al listei
                                --din Just String in String
                                then Just (map (fromJust) tempResult)
                                else Nothing
                                                

--Verifica daca o functie cu prototipul dat exista intr-o anumita clasa/clasele parinte
checkAllBaseClasses :: String -> String -> [String] -> Program -> Maybe String        
--Cazul de baza este cand am ajuns la clasa Global         
--Verificam daca in "Global" exista functia                                        
checkAllBaseClasses "Global" functionName arguments program = findFunction "Global" functionName arguments program

--Verificam pornind de la clasa folosita pentru apel
checkAllBaseClasses varClass functionName arguments program = 
        --verificam daca am gasit functia in clasa curenta
        case(findFunction varClass functionName arguments program) of
                --am gasit functia, intoarcem rezultatul evaluarii
                Just returnType -> Just returnType
                --nu am gasit functia, cautam in clasele parinte
                Nothing -> checkAllBaseClasses (getParentClass varClass program) functionName arguments program  

--Verifica daca intr-o clasa exista o functie cu un anumit prototip
findFunction  :: String -> String -> [String] -> Program -> Maybe String                                                               
findFunction varClass functionName arguments program = 
        --verifica daca exista o functie/macar o functie(in cazul celor supraincarcate)
        --care are prototipul corect
        if matchingArguments == []
            --nu exista un match
            then Nothing
            --intoarce primul rezultat
            else Just ((matchingArguments !! 0) !! 1)
        where
            --lista cu toate prototipurile de functii pe care vrem sa le verificam
            potentialArguments = (getFuncsForClass varClass program)
            --filter va lasa doar prototipurile functiilor care se potrivesc
            matchingArguments = filter (\args -> (drop 2 args) == arguments) potentialArguments


