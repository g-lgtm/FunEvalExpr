import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual )

import Lib ( evalExpr,
             simplifyOp,
             doAddSub,
             myDiv,
             myMul,
             myPowe,
             getNoPar,
             getJustPar,
             toList,
             cleanList,
             isAuthorized,
             isOperator,
             evalExpr,
             isSpeOp)

parseTest :: TestTree
parseTest = testCase "simplification of \"+\" and \"-\"" $ do
    assertEqual "Doing +++++++++---+- " ["+"] $
        simplifyOp ["+","+","+","+","+","+","+","+","+","-","-","-","+","-"]
    assertEqual "Doing +++++++++----- " ["-"] $
        simplifyOp ["+","+","+","+","+","+","+","+","+","-","-","-","-","-"]

addSubTest :: TestTree
addSubTest = testCase "adition and substraction" $ do
    assertEqual "Doing 2+2" ["4.0"] $
        doAddSub ["2","+","2"] []
    assertEqual "Doing 2-2" ["0.0"] $
        doAddSub ["2","-","2"] []

divTest :: TestTree
divTest = testCase "division" $ do
    assertEqual "Doing 9/2" "4.5" $
        myDiv "9" "2"
    assertEqual "Doing 9/0" "E" $
        myDiv "9" "0"

mulTest :: TestTree
mulTest = testCase "multiplication" $ do
    assertEqual "Doing 9 * 1.5" "13.5" $
        myMul "9" "1.5"
    assertEqual "Doing 9*0" "0.0" $
        myMul "9" "0"

poweTest :: TestTree
poweTest = testCase "\"^\"(power)" $ do
    assertEqual "Doing 9 ^2" "81.0" $
        myPowe "9" "2"
    assertEqual "Doing 9 ^(-1)" "0.11111111" $
        myPowe "9" "-1"

noParTest :: TestTree
noParTest = testCase "parentheses" $ do
    assertEqual "Doing par (2 + 5) * 9" ["*","9"] $
        getNoPar ["2","+","5",")","*","9"] 0
    assertEqual "Doing par (2 + 5) * 9" ["2","+","5"] $
        getJustPar ["2","+","5",")","*","9"] 0

listingTest :: TestTree
listingTest = testCase "listing" $ do
    assertEqual "Doing 91*(2+5)" ["91","*","","(","2","+","5",")",""] $
        toList "91*(2+5)" []
    assertEqual "Doing 9 *   2+ 5" ["9","*","2","+","5"] $
        cleanList $ toList "9 *   2+ 5" []

checkingTest :: TestTree
checkingTest = testCase "checkings" $ do
    assertEqual "isAuthorized B" False $
        isAuthorized 'B'
    assertEqual "isOperator ^" True $
        isOperator '^'
    assertEqual "isSpeOp +" False $ isSpeOp '+'

mainProgTest :: TestTree
mainProgTest = testCase "main program" $ do
    assertEqual "Doing 9 * (3 + 5.34)" ["75.06"] $
        evalExpr $ simplifyOp $ cleanList $ toList "9 * (3 + 5.34)" []
    assertEqual "Doing 9/(2*(2-1) + 5)" ["1.2857143"] $
        evalExpr $ simplifyOp $ cleanList $ toList "9/(2*(2-1) + 5)" []

main :: IO ()
main = defaultMain $ 
    testGroup "Unit Tests" [parseTest, addSubTest, divTest, mulTest, poweTest, 
    noParTest, listingTest, checkingTest, mainProgTest]