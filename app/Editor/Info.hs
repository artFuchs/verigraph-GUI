module Editor.Info
( Info
, infoLabel
, infoType
, infoSetLabel
, infoSetType
)where

-- An info is a string in the format "label{type}"
type Info = String

infoLabel :: Info -> String
infoLabel info = unwords . words $ infoLabelAux info

infoLabelAux :: Info -> String
infoLabelAux [] = []
infoLabelAux ('{':cs) = []
infoLabelAux (c:cs) = c : infoLabel cs

infoType :: Info -> String
infoType [] = []
infoType ('{':cs) = unwords . words $ infoTypeAux cs
infoType (c:cs) = infoType cs

infoTypeAux :: Info -> String
infoTypeAux [] = []
infoTypeAux ('}':cs) = []
infoTypeAux (c:cs) = c : infoTypeAux cs

infoSetLabel :: Info -> String -> Info
infoSetLabel i l = l ++ "{" ++ infoType i ++ "}"

infoSetType :: Info -> String -> Info
infoSetType i t = infoLabel i ++ "{" ++ t ++ "}"
