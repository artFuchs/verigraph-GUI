-- adapted from Verigraph Ctl tests
module Logic.Ltl.ModelCheckerSpec where


import qualified Data.Char   as Char
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List   as List
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Test.Hspec
import           Test.HUnit  hiding (State, Test)

import           Logic.Ltl
import           Logic.Model


spec :: Spec
spec = do

  context "with boolean expressions" $ do
    let
      model =
        assembleModel
          [ ["1 []"]
          , ["2 [p]"]
          , ["3 [q]"]
          , ["4 [p q]"]
          ]

      expr `shouldHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldBe` []

      expr `shouldNotHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldNotBe` []

    it "should handle 'true' properly" $
      "true" `shouldHoldOn` [1, 2, 3, 4]

    it "should handle 'false' properly" $
      "false" `shouldNotHoldOn` [1, 2, 3, 4]

    it "should handle 'p' properly" $
      "p" `shouldHoldOn` [2, 4]

    it "should handle 'q' properly" $
      "q" `shouldHoldOn` [3, 4]

    it "should handle '~p' properly" $
      "~p" `shouldHoldOn` [1, 3]

    it "should handle 'p && q' properly" $
      "p && q" `shouldHoldOn` [4]

    it "should handle 'p || q' properly" $
      "p || q" `shouldHoldOn` [2, 3, 4]

    it "should handle 'p -> q' properly" $
      "p -> q" `shouldHoldOn` [1, 3, 4]

    it "should handle 'p <-> q' properly" $
      "p <-> q" `shouldHoldOn` [1, 4]

    it "should handle '~(p <-> q)' properly" $
      "~(p <-> q)" `shouldHoldOn` [2, 3]


  describe "X" $ do
    let
      model =
        assembleModel
          [ ["1 []", "2 []", "3 [p]", "4 [p]"]
          , ["3 [p]", "2 []"]
          ]

      expr `shouldHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldBe` []

      expr `shouldNotHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldNotBe` []


    it "should handle 'X p' properly (1)" $
      "X p" `shouldHoldOn` [2, 4]

    it "should handle 'X p' properly (2)" $
      "X p" `shouldNotHoldOn` [3]

    it "should handle '~X p' properly" $
      "~X p" `shouldHoldOn` [1]




  describe "U" $ do
    let
      model =
        assembleModel
          [ ["1 []", "2 [p]", "3 [p]", "4 [q]", "5 [p]", "5 [p]"]
          , ["5 [p]", "3 [p]"]
          , ["3 [p]", "6 [p q]", "1 []"]
          ]

      expr `shouldHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldBe` []

    it "should handle 'p U q' properly" $
      "p U q" `shouldHoldOn` [2, 3, 4, 6]

    it "should handle '~(p U q)' properly" $
      "~(p U q)" `shouldHoldOn` [1]

  describe "F" $ do
    let
      model =
        assembleModel
          [ ["1 []", "2 []", "3 []", "4 [p]", "5 []"]
          , ["1 []", "1 []"]
          ]

      expr `shouldHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldBe` []

    it "should handle 'AF p' properly" $
      "F p" `shouldHoldOn` [2, 3, 4]



  describe "G" $ do
    let
      model =
        assembleModel
          [ ["1 []", "2 [p]", "3 [p]", "4 [p]"]
          , ["2 []", "2 []", "5 []"]
          , ["3 [p]", "3 [p]"]
          ]

      expr `shouldHoldOn` expectedStates =
        unsatisfyingPath expr model expectedStates `shouldBe` []

    it "should handle 'G p' properly" $
      "G p" `shouldHoldOn` [3, 4]






unsatisfyingPath :: String -> KripkeStructure String -> [Int] -> [Int]
unsatisfyingPath exprText model initialSts =
  case parseExpr "" exprText of
    Left err ->
      error ("Error parsing '"++ exprText ++"':\n"++ show err)

    Right expr ->
      List.sort (satisfyExpr model initialSts expr)

assembleModel :: [[String]] -> KripkeStructure String
assembleModel description =
  let
    (states, transitions') =
      parseLines description (IntMap.empty, Set.empty)

    (transitions, _) =
      Set.foldr addTransition ([], 0) transitions'

    addTransition (src, tgt) (ts, uid) =
      (Transition uid src tgt [] : ts, uid + 1)
  in
    KripkeStructure (IntMap.elems states) transitions


type PartialModel =
  (IntMap (State String), Set (Int, Int))


parseLines :: [[String]] -> PartialModel -> PartialModel
parseLines lines model =
  foldr parseLine model lines


parseLine :: [String] -> PartialModel -> PartialModel
parseLine stateTexts =
  addStates (map parseState stateTexts)


addStates :: [(Int, [String])] -> PartialModel -> PartialModel
addStates [] model =
  model

addStates [n1] (states, transitions) =
  (addState n1 states, transitions)

addStates (n1:n2:rest) (states, transitions) =
  let
    states' =
      addState n1 states

    transitions' =
      Set.insert (fst n1, fst n2) transitions
  in
    addStates (n2:rest) (states', transitions')


addState :: (Int, [String]) -> IntMap (State String) -> IntMap (State String)
addState (stateId, props) states =
  let
    addProps Nothing =
      State stateId props
    addProps (Just (State _ props')) =
      State stateId (props `List.union` props')
  in
    IntMap.alter (Just . addProps) stateId states


parseState :: String -> (Int, [String])
parseState text =
  let
    (stateId, text') =
      List.break Char.isSpace text

    ('[' : atomsText) =
      List.takeWhile (/= ']') $ List.dropWhile (/= '[') text'

    atoms =
      split Char.isSpace atomsText
  in
    (read stateId, atoms)


split :: (a -> Bool) -> [a] -> [[a]]
split _ [] =
  [[]]

split prop xs =
  let
    (first, rest') =
      List.break prop xs

    rest =
      List.dropWhile prop rest'
  in
    if List.null rest then
      [first, rest]
    else
      first : split prop rest
