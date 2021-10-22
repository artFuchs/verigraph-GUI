module GUI.Helper.Util (
  convertGrammar
) where

import           GI.Gtk      as Gtk
import           Data.IORef
import           Data.Int
import qualified Data.Map    as M
import           Data.Maybe

import           Abstract.Rewriting.DPO
import qualified Data.TypedGraph.Morphism  as TGM

import           GUI.Data.DiaGraph
import           GUI.Helper.GrammarMaker
import           GUI.Data.GraphState
import           GUI.Data.Info
import           GUI.Data.Nac
import qualified GUI.Editor.Helper.TreeStore as Edit

convertGrammar :: Gtk.TreeStore
             -> IORef (M.Map Int32 GraphState)
             -> IORef (M.Map Int32 (DiaGraph, MergeMapping))
             -> IO (Either String (Grammar (TGM.TypedGraphMorphism Info Info)))
convertGrammar store graphStates nacInfoMap = do
  sts <- readIORef graphStates

  let tg = stateGetGraph . fromJust $ M.lookup 0 sts
      hg = stateGetGraph . fromJust $ M.lookup 1 sts

  rules <- Edit.getRules store graphStates nacInfoMap
  let rulesNames = map (\(_,_,name) -> name) rules
      rulesNnacs = map (\(r,ns,_) -> (r,ns)) rules

  let efstOrderGG = makeGrammar tg hg rulesNnacs rulesNames
  return efstOrderGG
