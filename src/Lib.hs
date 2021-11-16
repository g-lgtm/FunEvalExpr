module Lib (checkArgs,
            toList,
            cleanList,
            isOperator,
            getJustPar,
            getNoPar,
            myPowe,
            isError,
            myAdd,
            myMul,
            myDiv,
            mySub,
            myDie,
            isPriorPar,
            isPriorPowe,
            isPriorMulDiv,
            doPriorPar,
            doPriorPowe,
            doPriorMulDiv,
            doAddSub,
            evalExpr,
            simplifyOp,
            checkResult,
            isAuthorized,
            isSpeOp) where

import System.Exit ( ExitCode(ExitFailure), exitSuccess, exitWith )
import System.IO ( stderr, hPutStrLn )
import Data.Char ( isDigit )

myDie :: String -> IO a
myDie str = hPutStrLn stderr str >> exitWith (ExitFailure 84)

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '/' = True
isOperator '*' = True
isOperator '^' = True
isOperator x = False

isAuthorized :: Char -> Bool
isAuthorized '(' = True
isAuthorized ')' = True
isAuthorized '.' = True
isAuthorized ' ' = True
isAuthorized '\t' = True
isAuthorized x | isOperator x = True
               | otherwise = False

isTab :: String -> Bool
isTab ('\\':'t':xs) = True
isTab ('\t':xs) = True
isTab str = False

withoutTab :: String -> String
withoutTab ('\\':'t':xs) = xs
withoutTab ('\t':xs) = xs
withoutTab str = str

checkMore :: [Char] -> IO()
checkMore [] = return ()
checkMore ['.'] = myDie "Syntax error"
checkMore str@(x:xs) | isDigit x || isAuthorized x = checkMore xs
                     | isTab str = checkMore $ withoutTab str
                     | otherwise = myDie "Please, type something correct."

isSpeOp :: Char -> Bool
isSpeOp '/' = True
isSpeOp '*' = True
isSpeOp '^' = True
isSpeOp x = False

checkPlace :: [Char] -> IO()
checkPlace [] = return ()
checkPlace (x:'.':y:xs) | isDigit y && isDigit x = checkPlace (y:xs)
                        | otherwise = myDie "Error, wrong arguments"
checkPlace (x:y:s) | isOperator x && isSpeOp y = myDie "Error"
                   | otherwise = checkPlace (y:s)
checkPlace (x:xs) = checkPlace xs

checkNbrDot :: [Char] -> Int -> IO()
checkNbrDot [] _ = return ()
checkNbrDot ('.':xs) 0 = checkNbrDot xs 1
checkNbrDot ('.':xs) nbr = myDie "Really? a number with 2 dots ?"
checkNbrDot (x:xs) nbr | isDigit x = checkNbrDot xs nbr
                       | otherwise = checkNbrDot xs 0

checkOrderPar :: [Char] -> Int -> Bool
checkOrderPar [] 0 = True
checkOrderPar [] nbr = False
checkOrderPar ('(':xs) nbr = checkOrderPar xs (nbr + 1)
checkOrderPar (')':xs) 0 = False
checkOrderPar (')':xs) nbr = checkOrderPar xs (nbr - 1)
checkOrderPar (x:xs) nbr = checkOrderPar xs nbr

checkNbrPar :: [Char] -> IO()
checkNbrPar all | checkOrderPar all 0 = return()
                | otherwise = myDie "NOP"

checkArgs :: [String] -> IO()
checkArgs [] = myDie "You have to put at least one argument"
checkArgs [""] = myDie "No number"
checkArgs [x] = checkMore x >> checkPlace x >> checkNbrDot x 0 >> checkNbrPar x
checkArgs x = myDie "Only one please"

cleanList :: [String] -> [String]
cleanList [] = []
cleanList ("":xs) = cleanList xs
cleanList ("\\t":xs) = cleanList xs
cleanList ("\t":xs) = cleanList xs
cleanList ("    ":xs) = cleanList xs
cleanList (x:xs) = x : cleanList xs

toList :: String -> String -> [String]
toList [] save = [save]
toList (' ':xs) save = save : toList xs []
toList ('+':xs) save = save : "+" : toList xs []
toList ('-':xs) save = save : "-" : toList xs []
toList ('*':xs) save = save : "*" : toList xs []
toList ('/':xs) save = save : "/" : toList xs []
toList ('^':xs) save = save : "^" : toList xs []
toList ('(':xs) save = save : "(" : toList xs []
toList (')':xs) save = save : ")" : toList xs []
toList (x:xs) save = toList xs (save ++ [x])

getJustPar :: [String] -> Int -> [String]
getJustPar [] _ = []
getJustPar (")":xs) 0 = []
getJustPar (")":xs) nbr = ")" : getJustPar xs (nbr - 1)
getJustPar ("(":xs) nbr = "(" : getJustPar xs (nbr + 1)
getJustPar (x:xs) nbr = x : getJustPar xs nbr

getNoPar :: [String] -> Int -> [String]
getNoPar [] _ = []
getNoPar (")":xs) 0 = xs
getNoPar (")":xs) nbr = getNoPar xs (nbr - 1)
getNoPar ("(":xs) nbr = getNoPar xs (nbr + 1)
getNoPar (x:xs) nbr = getNoPar xs nbr

myPowe :: [Char] -> [Char] -> [Char]
myPowe [] _ = []
myPowe _ [] = []
myPowe a b = show $ (read a :: Float) ** (read b :: Float)

myMul :: [Char] -> [Char] -> [Char]
myMul [] _ = []
myMul _ [] = []
myMul a b = show $ (read a :: Float) * (read b :: Float)

myDiv :: [Char] -> [Char] -> [Char]
myDiv [] _ = []
myDiv _ [] = []
myDiv a b | (read b :: Float) == 0 = ['E']
          | otherwise = show $ (read a :: Float) / (read b :: Float)

isError :: [String] -> Bool
isError [] = False
isError ("E":xs) = True
isError (x:xs) = isError xs

myAdd :: [Char] -> [Char] -> [Char]
myAdd [] _ = []
myAdd _ [] = []
myAdd a b = show $ (read a :: Float) + (read b :: Float)

mySub :: [Char] -> [Char] -> [Char]
mySub [] _ = []
mySub _ [] = []
mySub a b = show $ (read a :: Float) - (read b :: Float)

isPriorPar :: [String] -> Bool
isPriorPar [] = False
isPriorPar ("(":xs) = True
isPriorPar (x:xs) = isPriorPar xs

isPriorPowe :: [String] -> Bool
isPriorPowe [] = False
isPriorPowe ("^":xs) = True
isPriorPowe (x:xs) = isPriorPowe xs

isPriorMulDiv :: [String] -> Bool
isPriorMulDiv [] = False
isPriorMulDiv ("*":xs) = True
isPriorMulDiv ("/":xs) = True
isPriorMulDiv (x:xs) = isPriorMulDiv xs

doPriorPar :: [String] -> [String] -> [String]
doPriorPar [] lst = lst
doPriorPar ("(":xs) lst = lst ++ evalExpr (getJustPar xs 0) ++ getNoPar xs 0
doPriorPar full@(x:xs) lst = doPriorPar xs $ lst ++ [x]

doPriorPowe :: [String] -> [String] -> [String]
doPriorPowe [] _ = []
doPriorPowe (x:"^":"-":y:xs) lst = lst ++ [myPowe x ("-" ++ y)] ++ xs
doPriorPowe (x:"^":y:xs) lst = lst ++ [myPowe x y] ++ xs
doPriorPowe (x:xs) lst = doPriorPowe xs $ lst ++ [x]

doPriorMulDiv :: [String] -> [String] -> [String]
doPriorMulDiv [] _ = []
doPriorMulDiv (x:"*":"-":y:xs) lst = lst ++ [myMul x ("-" ++ y)] ++ xs
doPriorMulDiv (x:"*":y:xs) lst = lst ++ [myMul x y] ++ xs
doPriorMulDiv (x:"/":"-":y:xs) lst = lst ++ [myDiv x ("-" ++ y)] ++ xs
doPriorMulDiv (x:"/":y:xs) lst = lst ++ [myDiv x y] ++ xs
doPriorMulDiv (x:xs) lst = doPriorMulDiv xs $ lst ++ [x]

doAddSub :: [String] -> [String] -> [String]
doAddSub [] _ = []
doAddSub (x:"+":y:xs) lst = lst ++ [myAdd x y] ++ xs
doAddSub (x:"-":y:xs) lst = lst ++ [mySub x y] ++ xs
doAddSub (x:xs) lst = doAddSub xs $ lst ++ [x]

evalExpr :: [String] -> [String]
evalExpr [] = []
evalExpr [x] = [x]
evalExpr ("+":xs) = evalExpr xs
evalExpr ("-":y:xs) = evalExpr $ ("-" ++ y) : xs
evalExpr all | isError all = ["E"]
             | isPriorPar all = evalExpr (doPriorPar all [])
             | isPriorPowe all = evalExpr (doPriorPowe all [])
             | isPriorMulDiv all = evalExpr (doPriorMulDiv all [])
             | otherwise = evalExpr (doAddSub all [])

simplifyOp :: [String] -> [String]
simplifyOp [] = []
simplifyOp ("+":"+":xs) = simplifyOp ("+" : xs)
simplifyOp ("+":"-":xs) = simplifyOp ("-" : xs)
simplifyOp ("-":"+":xs) = simplifyOp ("-" : xs)
simplifyOp ("-":"-":xs) = simplifyOp ("+" : xs)
simplifyOp (x:xs) = x : simplifyOp xs

checkResult :: [String] -> IO()
checkResult [] = myDie "Calcul error"
checkResult ("E":_) = myDie "Calcul error"
checkResult (x:xs) = return ()