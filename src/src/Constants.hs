module Constants where

strLitPrefix :: String
strLitPrefix = "str"

ptrPostfix :: String
ptrPostfix = "Ptr"

lengthAttr :: String
lengthAttr = "length"

lengthAttrPtr :: String
lengthAttrPtr = lengthAttr ++ ptrPostfix

arrStructPrefix :: String
arrStructPrefix = ".arr"

regArrStruct :: String
regArrStruct = "arrStruct"

regArr :: String
regArr = "arr"

regArrPtr :: String
regArrPtr = regArr ++ ptrPostfix

regElemPtr :: String
regElemPtr = regElem ++ ptrPostfix

regElem :: String
regElem = "elem"

regObj :: String
regObj = "obj"

entryLabel :: String
entryLabel = "entry"

regIter :: String
regIter = ".iter"

regCond :: String
regCond = ".cond"

selfParam :: String
selfParam = ".self"

mkMethodName :: String -> String -> String
mkMethodName cls method = cls ++ "." ++ method

constrLabel :: String
constrLabel = "new"

mkConstrLabel :: String -> String
mkConstrLabel cls = mkMethodName cls constrLabel

vtablePrefix :: String
vtablePrefix = ".vtable"

mkVtableName :: String -> String
mkVtableName cls = vtablePrefix ++ "." ++ cls

mkVtableType :: String -> String
mkVtableType cls = ".type" ++ mkVtableName cls



