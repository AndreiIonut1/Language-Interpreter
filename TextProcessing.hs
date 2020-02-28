module TextProcessing
where

--Primeste un sir si intoarce o lista de subsiruri prin impartirea sirului
--original dupa un delimitator
strtok :: String -> Char -> [String]
strtok inputString delim = strtokAux inputString delim [] False

--Primeste un sir si intoarce o lista de subsiruri prin impartirea sirului
--original dupa un delimitator, atat timp cat delimitatorul nu se afla intre ()
strtokExtended :: String -> Char -> [String]
strtokExtended inputString delim = strtokAux inputString delim [] True

--Primeste un sir care trebuie impartit, un delimitator, acumulatorul in care
--se construieste rezultatul(initial []), un bool care decide daca trebuie
--ignorat delimitatorul aflat intre ()  si intoarce o lista de subsiruri
strtokAux :: String -> Char -> [String] -> Bool -> [String]
strtokAux [] delim acc extended = acc
strtokAux inputString delim acc extended
        | index < strLength = (acc ++ [leftSubstring] ++ (strtokAux rightSubstring delim acc extended))
        --nu mai exista delimitatori in subsir, il adaugam pe tot in acumulator
        | otherwise = acc ++ [inputString]
        where
            strLength = length inputString
            index  = if extended 
                       --cauta urmatoarea pozitie a delimitatorului in afara () 
                       then findIndexIgnoringFuncs inputString delim
                        -- cauta urmatoarea pozitie a delimitatorului
                       else findIndex inputString delim
            --subsirul aflat inaintea primului delimitator valid           
            leftSubstring  = (take (index) inputString)
            --subsirul aflat dupa delimitator
            rightSubstring = drop (index + 1) inputString 

--Cauta pozitia in care un caracter se afla intr-un sir
--In cazul in care nu gaseste caracterul, va intoarce lungimea sirului pentru a semnala
--ca a ajuns la capat fara sa gaseasca charToFind
findIndex :: String -> Char -> Int
findIndex [] charToFind = 0
findIndex (firstChar:inputString) charToFind
    | firstChar == charToFind = 0
    | otherwise = 1 + findIndex inputString charToFind
    
--Cauta pozitia in care un caracter se afla intr-un sir, ignorand aparitiile dintre ()
--In cazul in care nu gaseste caracterul, va intoarce lungimea sirului pentru a semnala
--ca a ajuns la capat fara sa gaseasca charToFind    
findIndexIgnoringFuncs :: String -> Char -> Int
findIndexIgnoringFuncs inputString charToFind = findIndexAux inputString charToFind 0
        where 
            --primeste un Int suplimentar care retine cate paranteze deschise
            --exista la fiecare moment
            --este decrementat cand intalneste ')', adica una dintre paranteze
            --a fost inchisa
            findIndexAux :: String -> Char -> Int -> Int
            findIndexAux [] _ _ = 0
            findIndexAux (x:xs) charToFind openBrackets
                --o paranteze a fost inchisa
                | x == ')' =  1 + (findIndexAux xs charToFind (openBrackets - 1))
                --o paranteze a fost deschisa
                | x == '(' =  1 + (findIndexAux xs charToFind (openBrackets + 1))
                --exista paranteze deschise, ignora delimitatorii dintre ()
                | openBrackets > 0 = 1 + (findIndexAux xs charToFind openBrackets)
                --toate parantezele sunt inchise, verifica daca am dat de delimitator
                | x == charToFind = 0
                --caracterul curent nu e delimitator
                | otherwise = 1 + (findIndexAux xs charToFind openBrackets)
                                                                
--Verifica daca un sir contine un anumit caracter si intoarce True/False
contains :: String -> Char -> Bool
contains inputString charToFind
        --caracterul exista in sir, intoarce True       
        | findIndex inputString charToFind < length inputString = True
        | otherwise = False

--Sterge spatiile de la inceputul/finalul unui sir
--Strategia este sa spargem sirul primit dupa spatiu(' '), unde erau mai multe 
--spatii vor aparea siruri goale in lista rezultata, pe care le filtram
--Apoi trebuie doar sa combinam inapoi toate subsirurile rezultate  
trim :: String -> String
trim inputString = foldl combineSubstrings [] (filter nonEmptyStringPredicate (strtok inputString ' '))
                    where 
                        combineSubstrings acc x      = if (null acc) then x else acc ++ " " ++ x
                        nonEmptyStringPredicate list = length list > 0

--Inlocuieste : cu spatiu in sirul primit ca input
replaceSemicolons :: String -> String
replaceSemicolons inputString = foldr (\char acc -> if char == ':' then ' ':acc else char:acc) [] inputString                            
                                  
