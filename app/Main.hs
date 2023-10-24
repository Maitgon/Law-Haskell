module Main where

import qualified Data.Map as M
--import GeneticMax3SAT
import GeneticLaw
import Control.Monad

main :: IO ()
main = do

    input1 <- readFile "LAW_input/inputsBuenas/input1.txt"
    input2 <- readFile "LAW_input/inputsBuenas/input2.txt"
    input3 <- readFile "LAW_input/inputsBuenas/input3.txt"
    input4 <- readFile "LAW_input/inputsBuenas/input4.txt"
    input5 <- readFile "LAW_input/inputsBuenas/input5.txt"
    input6 <- readFile "LAW_input/inputsBuenas/input6.txt"

    ------------------
    -- LAW problem. --
    ------------------
    {-
    putStrLn "\nLAW problem solution:\n"

    input <- readFile "LAW_input/input1.txt"
    -- 50 iterations.
    (solO1, solOVal2, partiesN, lawsN) <- geneticLaw input 50 mutate2
    -- Output management.
    putStrLn $ unlines $ zipWith (\x y -> show x ++ "seats for: " ++ y) solO1 (M.elems partiesN)
    putStrLn $ show solOVal2 ++ " laws fullfilled by this distribution of seats."
    -}

    let iters = 500


    ----------------------
    -- Max3SAT problem. --
    ----------------------

    --putStrLn "\n\nMax3SAT problem solution:\n"

    --input' <- readFile "inputs/Max3SAT_input/input1.txt"
    -- 20 iterations.
    --(solO2, solOVal2) <- geneticMax3SAT input' 20
    -- Output management.
    --putStrLn $ unlines $ zipWith (\x y -> "x" ++ show x ++ " -> " ++ (if y == 1 then "True" else "False")) [0..] solO2
    --putStrLn $ show solOVal2 ++ " clauses satisfied"

    ---------------------
    -- Mutation 1 Law. --
    ---------------------

    putStrLn "\n\n-----------------"
    putStrLn "-- Mutation 1. --"
    putStrLn "-----------------"

    mut11 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input1 iters mutate1
        return y

    putStrLn "\nInput 1:"
    putStr $ prettyPrint $ frequency mut11
    putStrLn $ "Min/Max  = " ++ show (minimum mut11) ++ "/" ++ show (maximum mut11)
    putStrLn $ "Mean     = " ++ show (mean mut11)
    putStrLn $ "Variance = " ++ show (variance mut11)

    mut12 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input2 iters mutate1
        return y

    putStrLn "\nInput 2:"
    putStr $ prettyPrint $ frequency mut12
    putStrLn $ "Min/Max  = " ++ show (minimum mut12) ++ "/" ++ show (maximum mut12)
    putStrLn $ "Mean     = " ++ show (mean mut12)
    putStrLn $ "Variance = " ++ show (variance mut12)

    mut13 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input3 iters mutate1
        return y

    putStrLn "\nInput 3:"
    putStr $ prettyPrint $ frequency mut13
    putStrLn $ "Min/Max  = " ++ show (minimum mut13) ++ "/" ++ show (maximum mut13)
    putStrLn $ "Mean     = " ++ show (mean mut13)
    putStrLn $ "Variance = " ++ show (variance mut13)

    mut14 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input4 iters mutate1
        return y

    putStrLn "\nInput 4:"
    putStr $ prettyPrint $ frequency mut14
    putStrLn $ "Min/Max  = " ++ show (minimum mut14) ++ "/" ++ show (maximum mut14)
    putStrLn $ "Mean     = " ++ show (mean mut14)
    putStrLn $ "Variance = " ++ show (variance mut14)

    mut15 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input5 iters mutate1
        return y

    putStrLn "\nInput 5:"
    putStr $ prettyPrint $ frequency mut15
    putStrLn $ "Min/Max  = " ++ show (minimum mut15) ++ "/" ++ show (maximum mut15)
    putStrLn $ "Mean     = " ++ show (mean mut15)
    putStrLn $ "Variance = " ++ show (variance mut15)

    mut16 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input6 iters mutate1
        return y

    putStrLn "\nInput 6:"
    putStr $ prettyPrint $ frequency mut16
    putStrLn $ "Min/Max  = " ++ show (minimum mut16) ++ "/" ++ show (maximum mut16)
    putStrLn $ "Mean     = " ++ show (mean mut16)
    putStrLn $ "Variance = " ++ show (variance mut16)

    ---------------------
    -- Mutation 2 Law. --
    ---------------------

    putStrLn "\n\n-----------------"
    putStrLn "-- Mutation 2. --"
    putStrLn "-----------------"

    mut21 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input1 iters mutate2
        return y

    putStrLn "\nInput 1:"
    putStr $ prettyPrint $ frequency mut21
    putStrLn $ "Min/Max  = " ++ show (minimum mut21) ++ "/" ++ show (maximum mut21)
    putStrLn $ "Mean     = " ++ show (mean mut21)
    putStrLn $ "Variance = " ++ show (variance mut21)

    mut22 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input2 iters mutate2
        return y

    putStrLn "\nInput 2:"
    putStr $ prettyPrint $ frequency mut22
    putStrLn $ "Min/Max  = " ++ show (minimum mut22) ++ "/" ++ show (maximum mut22)
    putStrLn $ "Mean     = " ++ show (mean mut22)
    putStrLn $ "Variance = " ++ show (variance mut22)

    mut23 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input3 iters mutate2
        return y

    putStrLn "\nInput 3:"
    putStr $ prettyPrint $ frequency mut23
    putStrLn $ "Min/Max  = " ++ show (minimum mut23) ++ "/" ++ show (maximum mut23)
    putStrLn $ "Mean     = " ++ show (mean mut23)
    putStrLn $ "Variance = " ++ show (variance mut23)

    mut24 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input4 iters mutate2
        return y

    putStrLn "\nInput 4:"
    putStr $ prettyPrint $ frequency mut24
    putStrLn $ "Min/Max  = " ++ show (minimum mut24) ++ "/" ++ show (maximum mut24)
    putStrLn $ "Mean     = " ++ show (mean mut24)
    putStrLn $ "Variance = " ++ show (variance mut24)

    mut25 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input5 iters mutate2
        return y

    putStrLn "\nInput 5:"
    putStr $ prettyPrint $ frequency mut25
    putStrLn $ "Min/Max  = " ++ show (minimum mut25) ++ "/" ++ show (maximum mut25)
    putStrLn $ "Mean     = " ++ show (mean mut25)
    putStrLn $ "Variance = " ++ show (variance mut25)

    mut26 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input6 iters mutate2
        return y

    putStrLn "\nInput 6:"
    putStr $ prettyPrint $ frequency mut26
    putStrLn $ "Min/Max  = " ++ show (minimum mut26) ++ "/" ++ show (maximum mut26)
    putStrLn $ "Mean     = " ++ show (mean mut26)
    putStrLn $ "Variance = " ++ show (variance mut26)

    ---------------------
    -- Mutation 3 Law. --
    ---------------------

    putStrLn "\n\n-----------------"
    putStrLn "-- Mutation 3. --"
    putStrLn "-----------------"

    mut31 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input1 iters mutate3
        return y

    putStrLn "\nInput 1:"
    putStr $ prettyPrint $ frequency mut31
    putStrLn $ "Min/Max  = " ++ show (minimum mut31) ++ "/" ++ show (maximum mut31)
    putStrLn $ "Mean     = " ++ show (mean mut31)
    putStrLn $ "Variance = " ++ show (variance mut31)

    mut32 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input2 iters mutate3
        return y

    putStrLn "\nInput 2:"
    putStr $ prettyPrint $ frequency mut32
    putStrLn $ "Min/Max  = " ++ show (minimum mut32) ++ "/" ++ show (maximum mut32)
    putStrLn $ "Mean     = " ++ show (mean mut32)
    putStrLn $ "Variance = " ++ show (variance mut32)

    mut33 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input3 iters mutate3
        return y

    putStrLn "\nInput 3:"
    putStr $ prettyPrint $ frequency mut33
    putStrLn $ "Min/Max  = " ++ show (minimum mut33) ++ "/" ++ show (maximum mut33)
    putStrLn $ "Mean     = " ++ show (mean mut33)
    putStrLn $ "Variance = " ++ show (variance mut33)

    mut34 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input4 iters mutate3
        return y

    putStrLn "\nInput 4:"
    putStr $ prettyPrint $ frequency mut34
    putStrLn $ "Min/Max  = " ++ show (minimum mut34) ++ "/" ++ show (maximum mut34)
    putStrLn $ "Mean     = " ++ show (mean mut34)
    putStrLn $ "Variance = " ++ show (variance mut34)

    mut35 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input5 iters mutate3
        return y

    putStrLn "\nInput 5:"
    putStr $ prettyPrint $ frequency mut35
    putStrLn $ "Min/Max  = " ++ show (minimum mut35) ++ "/" ++ show (maximum mut35)
    putStrLn $ "Mean     = " ++ show (mean mut35)
    putStrLn $ "Variance = " ++ show (variance mut35)

    mut36 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input6 iters mutate3
        return y

    putStrLn "\nInput 6:"
    putStr $ prettyPrint $ frequency mut36
    putStrLn $ "Min/Max  = " ++ show (minimum mut36) ++ "/" ++ show (maximum mut36)
    putStrLn $ "Mean     = " ++ show (mean mut36)
    putStrLn $ "Variance = " ++ show (variance mut36)


    ---------------------
    -- Mutation 4 Law. --
    ---------------------

    putStrLn "\n\n-----------------"
    putStrLn "-- Mutation 4. --"
    putStrLn "-----------------"

    mut41 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input1 iters mutate4
        return y

    putStrLn "\nInput 1:"
    putStr $ prettyPrint $ frequency mut41
    putStrLn $ "Min/Max  = " ++ show (minimum mut41) ++ "/" ++ show (maximum mut41)
    putStrLn $ "Mean     = " ++ show (mean mut41)
    putStrLn $ "Variance = " ++ show (variance mut41)

    mut42 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input2 iters mutate4
        return y

    putStrLn "\nInput 2:"
    putStr $ prettyPrint $ frequency mut42
    putStrLn $ "Min/Max  = " ++ show (minimum mut42) ++ "/" ++ show (maximum mut42)
    putStrLn $ "Mean     = " ++ show (mean mut42)
    putStrLn $ "Variance = " ++ show (variance mut42)

    mut43 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input3 iters mutate4
        return y

    putStrLn "\nInput 3:"
    putStr $ prettyPrint $ frequency mut43
    putStrLn $ "Min/Max  = " ++ show (minimum mut43) ++ "/" ++ show (maximum mut43)
    putStrLn $ "Mean     = " ++ show (mean mut43)
    putStrLn $ "Variance = " ++ show (variance mut43)

    mut44 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input4 iters mutate4
        return y

    putStrLn "\nInput 4:"
    putStr $ prettyPrint $ frequency mut44
    putStrLn $ "Min/Max  = " ++ show (minimum mut44) ++ "/" ++ show (maximum mut44)
    putStrLn $ "Mean     = " ++ show (mean mut44)
    putStrLn $ "Variance = " ++ show (variance mut44)

    mut45 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input5 iters mutate4
        return y

    putStrLn "\nInput 5:"
    putStr $ prettyPrint $ frequency mut45
    putStrLn $ "Min/Max  = " ++ show (minimum mut45) ++ "/" ++ show (maximum mut45)
    putStrLn $ "Mean     = " ++ show (mean mut45)
    putStrLn $ "Variance = " ++ show (variance mut45)

    mut46 <- replicateM 1000 $ do
        (_,y,_,_) <- geneticLaw input6 iters mutate4
        return y

    putStrLn "\nInput 6:"
    putStr $ prettyPrint $ frequency mut46
    putStrLn $ "Min/Max  = " ++ show (minimum mut46) ++ "/" ++ show (maximum mut46)
    putStrLn $ "Mean     = " ++ show (mean mut46)
    putStrLn $ "Variance = " ++ show (variance mut46)


frequency :: [Int] -> M.Map Int Int
frequency xs = M.fromListWith (+) [(x, 1) | x <- xs]

prettyPrint :: M.Map Int Int -> String
prettyPrint m = M.foldlWithKey f "Appearances:\n" m
    where f result k a = result ++ "" ++ show k ++ " -> " ++ show a ++ "\n"

mean :: [Int] -> Float
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

variance :: [Int] -> Float
variance xs = sum (zipWith (*) aux2 aux2) / fromIntegral (length xs)
    where meant = mean xs
          aux1 = map fromIntegral xs
          aux2 = [x - meant | x <- aux1]
