module Editor.Info(
  Info
, infoLabel
, infoType
, infoOperation
, infoSetLabel
, infoSetType
, infoSetOperation
)where

-- An info is a string in the format "label{type}operation", where
--    label is the text of the node/edge
--    type is a label of a node/edge of the typegraph
--    operation is: "", "new" or "del"
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

infoOperation :: Info -> String
infoOperation [] = []
infoOperation ('}':cs) = unwords . words $ cs
infoOperation (c:cs) = infoOperation cs

infoSetLabel :: Info -> String -> Info
infoSetLabel i l = l ++ "{" ++ infoType i ++ "}" ++ infoOperation i

infoSetType :: Info -> String -> Info
infoSetType i t = infoLabel i ++ "{" ++ t ++ "}" ++ infoOperation i

infoSetOperation :: Info -> String -> Info
infoSetOperation i o = infoLabel i ++ "{" ++ infoType i ++ "}" ++ o
