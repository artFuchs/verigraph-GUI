module Editor.Data.Info(
  Info (..)
, InfoOperation (..)
, InfoLabel (..)
, empty
, info2Str
, str2Info
, infoSetLabel
, infoSetOperation
, infoSetType
, infoSetLocked
, infoLabelStr
, infoAddLabel
, infoOriginalLabel
, infoOperationStr
, infoVisible
)where

import Data.Maybe
import Data.List

-- | label of a node or edge
-- A label can be a single label or a group of labels 
--  (the last being for joining elements in NACs).
-- A group of labels consists of pairs (key,label) where
--      - key is a Int. 
--          If the key is 0 then the label is the original label of the element
--      - label is a String
data InfoLabel =  Label String 
                | LabelGroup [(Int,String)] 
                deriving (Show, Read, Eq)

-- | Operation applyed to an node or edge
data InfoOperation = Preserve 
                    | Create 
                    | Delete 
                    deriving (Show, Read, Eq)

-- | Information that a node or edge may be carrying
data Info = Info 
                { infoLabel :: InfoLabel
                , infoOperation :: InfoOperation
                , infoType :: String
                , infoLocked :: Bool
                } deriving (Show, Read, Eq)

-- | empty info for convenience
empty :: Info
empty = 
    Info
    { infoLabel = Label ""
    , infoOperation = Preserve
    , infoType = ""
    , infoLocked = False
    }


-- | convert an Info to a string in the format "[operation:]label{type}", where
--    [operation:]label is the text of the node/edge 
--      and '[' and ']' indicates that the text is optional 
--      (these characteres should not be part of the string)
--    type is a label of a node/edge of the typegraph
--    operation is : "", "new" or "del"
info2Str :: Info -> String
info2Str info = opStr ++ lblStr ++ tStr
    where 
        opStr = infoOperationStr info
        lblStr = case infoLabel info of
            Label str -> str
            LabelGroup lbls -> concat $ map (\(k,str) -> '#':(show k) ++ str ++ "\n") lbls
        tStr = "{" ++ infoType info ++ "}"

-- | Parse a string in the format "[operation:]label{type}", generating a info
str2Info :: String -> Info
str2Info str = Info lbl op t False
    where
        (op,str') = parseOperation str
        (lbl,str'') = parseLabel str'
        t = parseType str''

-- auxiliar functions to str2Info --------------------

parseOperation :: String -> (InfoOperation,String)
parseOperation str = case parseOperationAux str of
    ("new",rest) -> (Create,rest)
    ("del",rest) -> (Delete,rest)
    ("",rest)    -> (Preserve,rest)
    _ ->            (Preserve,str)

parseOperationAux :: String -> (String,String)
parseOperationAux "" = ("","")
parseOperationAux (':':cs) = ("",cs)
parseOperationAux (c:cs) = let (op',rest) = parseOperationAux cs 
                           in (c:op', rest)

parseLabel :: String -> (InfoLabel, String)
parseLabel "" = (Label "","")
parseLabel ('{':cs) = (Label "",cs) 
parseLabel (c:cs) = let (Label restlbl, rest) = parseLabel cs 
                    in (Label (c:restlbl),rest)

parseType :: String -> String
parseType "" = ""
parseType ('}':_) = ""
parseType (c:cs) = c : parseType cs

-- setters for Info -------------------------------------

infoSetLabel :: Info -> String -> Info
infoSetLabel info l =
    Info
    { infoLabel = Label l 
    , infoOperation = infoOperation info
    , infoType = infoType info
    , infoLocked = infoLocked info
    }

infoSetOperation :: Info -> InfoOperation -> Info
infoSetOperation info op =
    Info
    { infoLabel = infoLabel info
    , infoOperation = op
    , infoType = infoType info
    , infoLocked = infoLocked info
    }

infoSetType :: Info -> String -> Info
infoSetType info t = 
    Info
    { infoLabel = infoLabel info
    , infoOperation = infoOperation info
    , infoType = t
    , infoLocked = infoLocked info
    }

infoSetLocked :: Info -> Bool -> Info
infoSetLocked info l = 
    Info 
    { infoLabel = infoLabel info
    , infoOperation = infoOperation info
    , infoType = infoType info
    , infoLocked = l
    }

-- | Get the string from the label of the Info
infoLabelStr :: Info -> String
infoLabelStr info = case infoLabel info of
    Label str -> str
    LabelGroup lbls -> intercalate "\n" $ map snd lbls

-- | Add a new label for a Info
infoAddLabel :: Info -> Int -> String -> Info
infoAddLabel info k lbl = 
    Info
    { infoLabel = labels
    , infoOperation = infoOperation info
    , infoType = infoType info
    , infoLocked = infoLocked info
    }
    where
        labels = case infoLabel info of
            Label str -> LabelGroup [(0,str), (k,lbl)]
            LabelGroup lbls -> LabelGroup $ sortOn fst $ (k,lbl) : lbls

-- | Get the first label added in the Info
infoOriginalLabel :: Info -> String
infoOriginalLabel info = case infoLabel info of
    Label str -> str
    LabelGroup ls -> fromMaybe "" $ lookup 0 ls

-- | Get a string representing the infoOperation
infoOperationStr :: Info -> String
infoOperationStr info = case infoOperation info of
            Preserve -> ""
            Create -> "new:" 
            Delete -> "del:"

-- | Get a string in the format [operation:]label
infoVisible :: Info -> String
infoVisible info = (infoOperationStr info) ++ (infoLabelStr info)




