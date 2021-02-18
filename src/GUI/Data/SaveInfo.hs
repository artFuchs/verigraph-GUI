module GUI.Data.SaveInfo (
    SaveInfo(..)
)where

import qualified Data.Map as M
import           Data.Int

import           Data.Graphs
import           GUI.Data.GraphicalInfo
import           GUI.Data.GraphState
import           GUI.Data.Info
import           GUI.Data.Nac (NacInfo)

data SaveInfo = Topic String 
              | TypeGraph Int32 String GraphState 
              | HostGraph Int32 String GraphState 
              | RuleGraph Int32 String GraphState Bool 
              | NacGraph Int32 String NacInfo 
              deriving (Show,Read)