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
