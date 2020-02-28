module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import qualified Data.Map as Map
import Debug.Trace

-- Definire Program
type Program = Map.Map String ClassState

initEmptyProgram :: Program
initEmptyProgram = Map.insert "Global" (setParent initEmptyClass "Global") Map.empty

getVars :: Program -> [[String]]
getVars program = getValues  (fromJust $ Map.lookup "Global" program) Var
                    
getClasses :: Program -> [String]
getClasses program = Map.keys program

getParentClass :: String -> Program -> String
getParentClass className program = snd $ fromJust $ Map.lookup className program

getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass className program = case (Map.lookup className program) of
                                        Just myMap -> getValues myMap Func
                                        Nothing -> []

{-
    NewClass  nume parinte
    NewVar    numeVariabila numeClasa
    NewFunc   tipReturnat simbolClasa simbolFunctie listaTipParamString
-}
data Instruction = NoInstr |  NewClass String String | NewVar String String | NewFunc String String String [String] | Infer String Expr deriving Show

strtok :: String -> Char -> [String]
strtok [] delim = [""]
strtok inputString delim
    | stringHead == delim = if null(stringTail) 
                                then [""]
                                else []:(strtok stringTail delim)
    | otherwise = [[stringHead] ++ head rest] ++ tail rest
    where
        stringHead = head inputString
        stringTail = tail inputString
        rest = strtok stringTail delim   

trim :: String -> String
trim inputString = foldl combineSublists [] (filter emptyListPredicate (strtok inputString ' '))
                        where 
                            combineSublists acc x =  if (null acc) then x else acc ++ " " ++ x
                            emptyListPredicate list = length list > 0
 
stringToInstruction :: String -> Instruction                                   
stringToInstruction inputString = if not (null (head argumentsList)) then
                                    case (head argumentsList) of 
                                        "class" -> buildClassInstruction inputString
                                        "newvar" -> buildVarInstruction inputString
                                        "infer" -> buildInferInstruction inputString
                                        _ -> buildFuncInstruction inputString
                                  else 
                                    NoInstr   
                                where
                                     argumentsList = strtok inputString ' '      

findIndex :: String -> Char -> Int
findIndex [] c = 0
findIndex (x:xs) c = if x == c
                        then 0
                        else 1 + findIndex xs c

contains :: String -> Char -> Bool
contains [] c = False
contains (x:xs) c = if x == c
                        then True
                        else (contains xs c) 
                        
findIndexIgnoringFuncs :: String -> Char -> Int
findIndexIgnoringFuncs l c = findIndexAux l c 0
                             where findIndexAux [] _ _ = 0
                                   findIndexAux (x:xs) c open = if x == ')' then 1 + (findIndexAux xs c (open - 1))
                                                                else if  x == '(' then  1 + (findIndexAux xs c (open + 1))
                                                                else if open > 0 then 1 + (findIndexAux xs c (open))
                                                                else if x == c then 0
                                                                else 1 + (findIndexAux xs c open)

strtokExtended :: String -> Char -> [String]
strtokExtended inputString delim = strtokExtendedAux inputString delim []
                                   where
                                        strtokExtendedAux [] delim acc = acc
                                        strtokExtendedAux inputString delim acc
                                            | index < length inputString = (acc ++ [(take (index) inputString)] ++ (strtokExtendedAux (drop (index + 1) inputString) delim acc))
                                            | otherwise = acc ++ [inputString]
                                            where
                                                index = findIndexIgnoringFuncs inputString delim


replaceSemicolons :: String -> String
replaceSemicolons inputString = foldr(\x acc -> if x == ':' then ' ':acc else x:acc) [] inputString    

replaceEqual :: String -> String
replaceEqual inputString = foldr(\x acc -> if x == '=' then ' ':acc else x:acc) [] inputString                            

buildArgumentsList :: String -> [String]
buildArgumentsList inputString =  map trim (strtok parametersTemp2 ',')
                                where 
                                    parametersTemp1 = drop (findIndex inputString '(' + 1) inputString
                                    parametersTemp2 = take ((length parametersTemp1) - 1) parametersTemp1

buildFunctionHeader :: String -> [String]
buildFunctionHeader inputString = strtok (head $ map trim (strtok tempHeader2 ',')) ' '
                                where
                                    tempHeader1 = take (findIndex inputString '(') inputString
                                    tempHeader2 = replaceSemicolons tempHeader1

buildFuncInstruction :: String -> Instruction
buildFuncInstruction inputString = if length headerList /= 3 
                                then NoInstr
                                else NewFunc (headerList !! 0) (headerList !! 1) (headerList !! 2) argumentsList
                            where
                                 headerList    = buildFunctionHeader inputString
                                 argumentsList = buildArgumentsList inputString      

buildVarInstruction :: String -> Instruction                                 
buildVarInstruction inputString = NewVar varName className
                                        where
                                            tokens = map (trim) (strtok inputString '=')
                                            className = tokens !! 1
                                            varName = (strtok (tokens !! 0) ' ') !! 1

buildClassInstruction :: String -> Instruction
buildClassInstruction inputString = NewClass (className) (classParent)
                                where
                                    argumentsList = strtok inputString ' '
                                    className = argumentsList !! 1
                                    classParent = if (length argumentsList > 2) 
                                                    then (argumentsList !! 3) 
                                                    else "Global"

buildInferInstruction :: String -> Instruction
buildInferInstruction inputString = Infer newVarName (buildFCall (arguments !! 1))
                        where
                           arguments = strtok inputString '='
                           newVarName  = trim $ drop (length "infer" + 1) (arguments !! 0)


buildFCall :: String -> Expr
buildFCall inputString = if (contains inputString '.') 
                                        then (FCall callerVarName functionName rest)
                                        else (Va inputString)
                                        where 
                                            argsTemp1 = drop (findIndex inputString '(' + 1) inputString
                                            argsTemp2 = take ((length argsTemp1) - 1) argsTemp1
                                            arguments = map trim (strtokExtended argsTemp2 ',')
                                            header = take (findIndex inputString '(') inputString
                                            callPattern   = map trim (strtok (header) '.')
                                            callerVarName = callPattern !! 0
                                            functionName  = callPattern !! 1
                                            rest = (map (buildFCall) arguments)


parse :: String -> [Instruction]
parse inputString = (map (stringToInstruction) (map (trim) (strtok inputString '\n')))

interpret :: Instruction -> Program -> Program
interpret instruction program = case instruction of
                                    (NewClass  className classParent) -> addNewClass className classParent program
                                    (NewVar    varName   className) -> addNewVar varName className program
                                    (NewFunc   returnType className functionName argumentsList) -> (addNewFunc returnType className functionName argumentsList program)
                                    (Infer     varName expr) -> case(infer expr program) of 
                                                                    Just dataType -> interpret ((parse ("newvar " ++ varName ++ " = " ++ dataType)) !! 0) program
                                                                    Nothing -> program
                                                                    
                                    NoInstr -> program

addNewClass :: String -> String -> Program -> Program
addNewClass className classParent program
                                    | Map.member className program == False = if Map.member classParent program
                                                                                then Map.insert className (setParent initEmptyClass classParent) program
                                                                                else Map.insert className (setParent initEmptyClass "Global") program
                                    | otherwise = program

addNewVar :: String -> String -> Program -> Program
addNewVar varName className program
                           | Map.member className program == True = case(Map.lookup "Global" program) of
                                        Just myMap -> Map.insert "Global" (insertIntoClass (myMap) Var [varName, className]) program
                           | otherwise = program

addNewFunc :: String -> String -> String -> [String] -> Program -> Program
addNewFunc returnType className functionName argumentsList program 
                           | Map.member className program == True = case(Map.lookup className program) of
                                        Just myMap -> if (validateFuncArgs argumentsList program) &&  (validateRet returnType program)
                                                        then Map.insert className (insertIntoClass (myMap) Func arguments) program
                                                        else program
                           | otherwise = program   
                           where
                           arguments = if (argumentsList /= [[]]) 
                                        then ([functionName, returnType] ++ argumentsList)
                                        else  ([functionName, returnType])   

validateFuncArgs :: [String] -> Program -> Bool
validateFuncArgs argumentsList program 
        | (argumentsList /= [""]) == True = all (\x -> validateRet x program) argumentsList
        | otherwise = True
    

validateRet :: String -> Program -> Bool
validateRet returnType program = Map.member returnType program

infer :: Expr -> Program -> Maybe String
infer (Va varName) program = getVarClass varName (fromJust $ Map.lookup "Global" program)
infer (FCall varName functionName arguments) program = case (infer (Va varName) program) of
                                                          Just varClass -> case (inferResult) of
                                                                            Just inferArgs -> checkAllBaseClasses varClass functionName inferArgs program
                                                                            Nothing -> Nothing
                                                          Nothing -> Nothing
                                                       where
                                                            inferResult = if (all (\x -> x /= Nothing) tempResult) 
                                                                                then Just (map (fromJust) tempResult)
                                                                                else Nothing
                                                            tempResult  = (map (\e -> infer e program) arguments)
                                                
   
checkAllBaseClasses :: String -> String -> [String] -> Program -> Maybe String                                                         
checkAllBaseClasses "Global" functionName arguments program = findFunction "Global" functionName arguments program
checkAllBaseClasses varClass functionName arguments program = case(findFunction varClass functionName arguments program) of
                                                                    Just returnType -> Just returnType
                                                                    Nothing -> checkAllBaseClasses (getParentClass varClass program) functionName arguments program  

findFunction  :: String -> String -> [String] -> Program -> Maybe String                                                               
findFunction varClass functionName arguments program = if matchingArguments == []
                                                                then Nothing
                                                                else Just ((matchingArguments !! 0) !! 1) --ia primul match
                                                        where
                                                            potentialArguments = (getFuncsForClass varClass program)
                                                            matchingArguments = filter (\l -> (drop 2 l) == arguments) potentialArguments


