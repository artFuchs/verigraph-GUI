module Editor.Data.Info(
  Info
, infoVisible
, infoLabel
, infoType
, infoOperation
, infoLocked
, infoSetLabel
, infoSetType
, infoSetOperation
, infoSetLocked
)where

-- An info is a string in the format "[operation:]label{type}[L]", where
--    [operation:]label is the text of the node/edge
--    type is a label of a node/edge of the typegraph
--    operation is: "", "new" or "del"
--    [L] indicates if the object is locked
type Info = String

infoVisible :: Info -> String
infoVisible info = unwords . words $ infoVisibleAux info

infoVisibleAux :: Info -> String
infoVisibleAux [] = []
infoVisibleAux ('{':cs) = []
infoVisibleAux (c:cs) = c : infoVisibleAux cs

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

infoLocked :: Info -> Bool
infoLocked info = if (null info) then False
                                 else (last info) == 'L'

infoSetLabel :: Info -> String -> Info
infoSetLabel i l = let i' = case infoOperation i of
                              [] -> l ++ "{" ++ infoType i ++ "}"
                              o  -> o ++ ":" ++ l ++ "{" ++ infoType i ++ "}"
                   in infoSetLocked i' (infoLocked i)


infoSetType :: Info -> String -> Info
infoSetType i t = let i' = case infoOperation i of
                              [] -> infoLabel i ++ "{" ++ t ++ "}"
                              o  -> o ++ ":" ++ infoLabel i ++ "{" ++ t ++ "}"
                  in infoSetLocked i' (infoLocked i)

infoSetOperation :: Info -> String -> Info
infoSetOperation i "" = infoSetLocked (infoLabel i ++ "{" ++ infoType i ++ "}") (infoLocked i)
infoSetOperation i o = infoSetLocked (o ++ ":" ++ infoLabel i ++ "{" ++ infoType i ++ "}") (infoLocked i)

infoSetLocked :: Info -> Bool -> Info
infoSetLocked info True = case infoLocked info of
  True -> info
  False -> info ++ "L"
infoSetLocked info False = case infoLocked info of
  False -> info
  True -> reverse $ tail (reverse info)
