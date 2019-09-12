module Editor.Data.Info(
  Info
, infoVisible
, infoLabel
, infoType
, infoOperation
, infoSetLabel
, infoSetType
, infoSetOperation
)where

-- An info is a string in the format "[operation:]label{type}", where
--    [operation:]label is the text of the node/edge
--    type is a label of a node/edge of the typegraph
--    operation is: "", "new" or "del"
type Info = String

infoVisible :: Info -> String
infoVisible info = unwords . words $ infoVisibleAux info

infoVisibleAux :: Info -> String
infoVisibleAux [] = []
infoVisibleAux ('{':cs) = []
infoVisibleAux (c:cs) = c : infoVisible cs

infoLabel :: Info -> String
infoLabel [] = []
infoLabel i = case infoOperation i of
  [] -> infoVisible i
  o -> infoLabelAux . infoVisible $ i

infoLabelAux :: String -> String
infoLabelAux [] = []
infoLabelAux (':':cs) = cs
infoLabelAux (c:cs) = infoLabelAux cs

infoType :: Info -> String
infoType [] = []
infoType ('{':cs) = unwords . words $ infoTypeAux cs
infoType (c:cs) = infoType cs

infoTypeAux :: Info -> String
infoTypeAux [] = []
infoTypeAux ('}':cs) = []
infoTypeAux (c:cs) = c : infoTypeAux cs

infoOperation :: Info -> String
infoOperation info = case infoOperationAux info of
  Nothing -> []
  Just cs -> cs

infoOperationAux :: Info -> Maybe String
infoOperationAux [] = Nothing
infoOperationAux (':':cs) = Just []
infoOperationAux ('{':cs) = Nothing
infoOperationAux (c:cs) = Just (c:) <*> infoOperationAux cs

infoSetLabel :: Info -> String -> Info
infoSetLabel i l = case infoOperation i of
  [] -> l ++ "{" ++ infoType i ++ "}"
  o  -> o ++ ":" ++ l ++ "{" ++ infoType i ++ "}"

infoSetType :: Info -> String -> Info
infoSetType i t = case infoOperation i of
  [] -> infoLabel i ++ "{" ++ t ++ "}"
  o  -> o ++ ":" ++ infoLabel i ++ "{" ++ t ++ "}"

infoSetOperation :: Info -> String -> Info
infoSetOperation i "" = infoLabel i ++ "{" ++ infoType i ++ "}"
infoSetOperation i o = o ++ ":" ++ infoLabel i ++ "{" ++ infoType i ++ "}"