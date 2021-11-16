module Main where

import System.Environment ( getArgs )
import Data.List ( (++) )
import Lib
    ( checkArgs,
      cleanList,
      getJustPar,
      getNoPar,
      isError,
      myAdd,
      myDie,
      myDiv,
      myMul,
      myPowe,
      mySub,
      toList,
      simplifyOp,
      evalExpr,
      checkResult,)
import Text.Printf ( printf )
import Data.Char ( isDigit )
import Prelude

isNumber :: String -> Bool
isNumber [] = False
isNumber (x:xs) | isDigit x = True
                | otherwise = False

isNumberIn :: [String] -> [String] -> [String]
isNumberIn [] _ = ["E"]
isNumberIn (x:xs) all | isNumber x = all
                      | otherwise = isNumberIn xs all

getRidOfTab :: [Char] -> [Char]
getRidOfTab [] = []
getRidOfTab ('\\':'t':xs) = getRidOfTab xs
getRidOfTab ('\t':xs) = getRidOfTab xs
getRidOfTab (x:xs) = x:getRidOfTab xs

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    let tmp = getRidOfTab $ head args
    let argLst = simplifyOp $ cleanList $ toList tmp []
    let result = evalExpr $ isNumberIn argLst argLst
    checkResult result
    printf "%.2f\n" (read (head result) :: Float)