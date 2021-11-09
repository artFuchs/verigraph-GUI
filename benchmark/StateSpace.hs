module Main where

import Test.Tasty.Bench
import System.Random
import Control.Monad

import GUI.Analysis.ModelChecker.StateSpace

import            Abstract.Rewriting.DPO
import qualified  Abstract.Category        as Cat
import qualified  Data.TypedGraph.Morphism as TGM
import qualified  Data.TypedGraph          as TG
import            Data.Graphs
import            Data.Graphs.Morphism
import qualified  Data.Relation            as R
import qualified  Abstract.Rewriting.DPO.StateSpace as SS

import GUI.Data.Info

main :: IO ()
main = do
  defaultMain
    [ bgroup "Explore state space"
      [ bench "Initial Graph with 2 nodes " $ whnfIO (explore initial5)
      , bench "Initial Graph with 3 nodes " $ whnfIO (explore initial3)
      , bench "Initial Graph with 4 nodes " $ whnfIO (explore initial4)
      , bench "Initial Graph with 5 nodes " $ whnfIO (explore initial5)
      ]
    ]


explore graph = do
  (_,ss) <- exploreStateSpace mconf 1000 (grammar graph [] prods) graph Nothing
  putStrLn $ show (length $ SS.states ss) ++ " states"

mconf :: MorphismsConfig (TGM.TypedGraphMorphism Info Info)
mconf = MorphismsConfig Cat.monic

initial2 =
  GraphMorphism
    { domainGraph = Graph
      { nodeMap =
          [(NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})}),(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
      , edgeMap = []
      }
    , codomainGraph = Graph
      { nodeMap = [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      , edgeMap =
          [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      }
    , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
    , edgeRelation = R.fromLists [] [EdgeId 1] []
    }

initial3 =
  GraphMorphism
    { domainGraph = Graph
      { nodeMap =
        [ (NodeId 3,Node {nodeId = NodeId 3, nodeInfo = Just (Info {infoLabel = Label "3", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        , (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ]
      , edgeMap = []
      }
    , codomainGraph = Graph
      { nodeMap =
        [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      , edgeMap =
        [ (EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      }
    , nodeRelation = R.fromLists [NodeId 1,NodeId 2,NodeId 3] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1),(NodeId 3,NodeId 1)]
    , edgeRelation = R.fromLists [] [EdgeId 1] []
    }



initial4 =
  GraphMorphism
    { domainGraph = Graph
      { nodeMap =
          [ (NodeId 4,Node {nodeId = NodeId 4, nodeInfo = Just (Info {infoLabel = Label "4", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
          , (NodeId 3,Node {nodeId = NodeId 3, nodeInfo = Just (Info {infoLabel = Label "3", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
          , (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
          , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
          ]
      , edgeMap = []
      }
    , codomainGraph = Graph
      { nodeMap =
          [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      , edgeMap =
          [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      }
    , nodeRelation = R.fromLists [NodeId 1,NodeId 2,NodeId 3,NodeId 4] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1),(NodeId 3,NodeId 1),(NodeId 4,NodeId 1)]
    , edgeRelation = R.fromLists [] [EdgeId 1] []
    }

initial5 =
  GraphMorphism
    { domainGraph = Graph
      { nodeMap =
        [(NodeId 5,Node {nodeId = NodeId 5, nodeInfo = Just (Info {infoLabel = Label "5", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ,(NodeId 4,Node {nodeId = NodeId 4, nodeInfo = Just (Info {infoLabel = Label "4", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ,(NodeId 3,Node {nodeId = NodeId 3, nodeInfo = Just (Info {infoLabel = Label "3", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ,(NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ,(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
        ]
      , edgeMap = []
      }
    , codomainGraph = Graph
      { nodeMap =
        [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      , edgeMap =
        [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
      }
    , nodeRelation =
      R.fromLists [NodeId 1,NodeId 2,NodeId 3,NodeId 4,NodeId 5] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1),(NodeId 3,NodeId 1),(NodeId 4,NodeId 1),(NodeId 5,NodeId 1)]
    , edgeRelation = R.fromLists [] [EdgeId 1] []
    }




prods =
  [("connect",
    Production
    { leftMorphism = TGM.TypedGraphMorphism
      { TGM.domainGraph = GraphMorphism
        { domainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap = []
          }
        , codomainGraph = Graph
          { nodeMap =
            [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap =
            [ (EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})
            ]
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
        , edgeRelation = R.fromLists [] [EdgeId 1] []
        }
      , TGM.codomainGraph = GraphMorphism
        { domainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap = []
          }
        , codomainGraph = Graph
          { nodeMap =
            [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap =
            [ (EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})
            ]
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
        , edgeRelation = R.fromLists [] [EdgeId 1] []
        }
      , TGM.mapping = GraphMorphism
        { domainGraph = Graph
          { nodeMap = [(NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})}),(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
          , edgeMap = []
          }
        , codomainGraph = Graph
          { nodeMap = [(NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})}),(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
          , edgeMap = []
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1,NodeId 2] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 2)]
        , edgeRelation = R.fromLists [] [] []
        }
      }
    , rightMorphism = TGM.TypedGraphMorphism
      { TGM.domainGraph = GraphMorphism
        { domainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap = []
          }
        , codomainGraph = Graph
          { nodeMap =
            [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})}) ]
          , edgeMap =
            [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
        , edgeRelation = R.fromLists [] [EdgeId 1] []
        }
      , TGM.codomainGraph = GraphMorphism
        { domainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap =
            [ (EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 2, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Create, infoType = "1", infoLocked = False, infoExtra = ""})})]
          }
        , codomainGraph = Graph
          { nodeMap =
            [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
          , edgeMap =
            [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
        , edgeRelation = R.fromLists [EdgeId 1] [EdgeId 1] [(EdgeId 1,EdgeId 1)]
        }
      , TGM.mapping = GraphMorphism
        { domainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
          , edgeMap = []
          }
        , codomainGraph = Graph
          { nodeMap =
            [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
            ]
          , edgeMap =
            [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 2, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Create, infoType = "1", infoLocked = False, infoExtra = ""})})]
          }
        , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1,NodeId 2] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 2)]
        , edgeRelation = R.fromLists [] [EdgeId 1] []
        }
      }
    , nacs =
      [ TGM.TypedGraphMorphism
        { TGM.domainGraph = GraphMorphism
          { domainGraph = Graph
            { nodeMap =
              [(NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})}),(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
            , edgeMap = []
            }
          , codomainGraph = Graph
            { nodeMap =
              [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
            , edgeMap =
              [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
            }
          , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
          , edgeRelation = R.fromLists [] [EdgeId 1] []
          }
        , TGM.codomainGraph = GraphMorphism
          { domainGraph = Graph
            { nodeMap =
              [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = True, infoExtra = ""})})
              , (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = True, infoExtra = ""})})
              ]
            , edgeMap =
              [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 2, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
            }
          , codomainGraph = Graph
            { nodeMap = [(NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
            , edgeMap = [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 1, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "", infoLocked = False, infoExtra = ""})})]
            }
          , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1] [(NodeId 1,NodeId 1),(NodeId 2,NodeId 1)]
          , edgeRelation = R.fromLists [EdgeId 1] [EdgeId 1] [(EdgeId 1,EdgeId 1)]
          }
        , TGM.mapping = GraphMorphism
          { domainGraph = Graph
            { nodeMap =
              [ (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
              , (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})
              ]
            , edgeMap = []
            }
          , codomainGraph = Graph
            { nodeMap =
              [ (NodeId 1,Node {nodeId = NodeId 1, nodeInfo = Just (Info {infoLabel = Label "2", infoOperation = Preserve, infoType = "1", infoLocked = True, infoExtra = ""})})
              , (NodeId 2,Node {nodeId = NodeId 2, nodeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = True, infoExtra = ""})})
              ]
            , edgeMap =
              [(EdgeId 1,Edge {edgeId = EdgeId 1, sourceId = NodeId 2, targetId = NodeId 1, edgeInfo = Just (Info {infoLabel = Label "1", infoOperation = Preserve, infoType = "1", infoLocked = False, infoExtra = ""})})]
            }
          , nodeRelation = R.fromLists [NodeId 1,NodeId 2] [NodeId 1,NodeId 2] [(NodeId 1,NodeId 2),(NodeId 2,NodeId 1)]
          , edgeRelation = R.fromLists [] [EdgeId 1] []
          }
        }
      ]
    }
  )]
