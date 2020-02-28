module ClassState
where

import Data.Maybe    
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq, Ord)

-- TODO - Trebuie definit ClassState
type ClassState = (Map.Map InstrType (Map.Map String [[String]]), String)

initEmptyClass :: ClassState
initEmptyClass = (Map.insert Func Map.empty (Map.insert Var Map.empty Map.empty), "")

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (classState, parent) instrType list = case (Map.lookup instrType classState) of 
                                                        Nothing -> (classState, parent)
                                                        Just myMap -> (Map.insert instrType (Map.insert key value myMap) classState, parent)
                                                                        where
                                                                            key = head list
                                                                            value = if Map.member key myMap
                                                                                        then ((fromJust $ Map.lookup key myMap) ++ [tail list])
                                                                                        else [tail list]

setParent :: ClassState -> String -> ClassState
setParent (classState, p) parent = (classState, parent)

getValues :: ClassState -> InstrType -> [[String]]
getValues (classState, parent) instrType = case (Map.lookup instrType classState) of
                                                Just myMap -> buildValuesList (Map.toList myMap)
                                                Nothing -> [[""]]


getVarClass varName (classState, parent) = case (Map.lookup Var classState) of
                                        Just myMap -> case(Map.lookup varName myMap) of
                                                        Just varClass -> Just ((varClass !! 0) !! 0) --din cauza functiilor si variabilele sunt stocate ca liste de liste
                                                        Nothing -> Nothing
                                        Nothing -> Nothing                                        

buildValuesList :: [(String, [[String]])] -> [[String]]
buildValuesList [] = []
buildValuesList ((functionName, l1):l) = (getOverloadedFunctions functionName l1) ++ (buildValuesList l)

getOverloadedFunctions :: String -> [[String]] -> [[String]]
getOverloadedFunctions functionName [] = []
getOverloadedFunctions functionName (l:l1) = [functionName : l] ++ (getOverloadedFunctions functionName l1)
