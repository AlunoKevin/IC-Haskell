module Main (main) where
--import "arvore.hs"
import Test.Tasty
import Test.Tasty.HUnit
import ArvoreBinaria.Arvore(ABB(..),searchABB, insertABB, removeABB, tamABB, hightABB, printABB)

main :: IO ()
main = defaultMain $ testGroup "All Tests" [testABBfunctions]


testABBfunctions :: TestTree
testABBfunctions = testGroup "ABB functions"
  [ testCase "searchABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 (insertABB 1 Vazio)))
      searchABB 3 tree @?= True
      searchABB 4 tree @?= False

  , testCase "insertABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 Vazio))
      tamABB tree @?= 3

  , testCase "removeABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 Vazio))
      let newTree = removeABB 3 tree
      searchABB 3 newTree @?= False
      tamABB newTree @?= 2

  , testCase "tamABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 Vazio))
      tamABB tree @?= 3

  , testCase "hightABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 Vazio))
      hightABB tree @?= 2

  , testCase "printABB" $ do
      let tree = insertABB 5 (insertABB 3 (insertABB 7 Vazio))
      printABB tree @?= "357"
  ]