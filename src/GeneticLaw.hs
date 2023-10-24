module GeneticLaw (
    geneticLaw
  , mutate1
  , mutate2
  , mutate3
  , mutate4
) where

import Data.List.Split (splitOn)
import Data.List ((\\), elemIndex, sortOn, foldl1', nub)
import System.Random (newStdGen, Random(randomRs, random, randomR))
import qualified Data.Map as M
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

population :: Int
population = 20
mutFactor1 :: Float
mutFactor1 = 0.1
mutFactor2 :: Float
mutFactor2 = 0.1
mutFactor3 :: Float
mutFactor3 = 0.1

geneticLaw :: String -> Int -> (Int -> [Int] -> IO [Int]) -> IO ([Int], Int, M.Map Int String, M.Map String String)
geneticLaw input iters mutation = do
    let [_,i1,i2,i3,i4,i5] = splitOnAnyOf splits input

    -- getting the values:
    let seats = read i1 :: Int
    let parties = length $ lines i2

    let partiesN = M.fromList $ map ((\[x,y] -> (read x :: Int,y)) . splitOn " : ") $ lines i2
    let lawsN = M.fromList $ map ((\[x,y] -> (x,y)) . splitOn " : ") $ lines i3

    let pro = getVotes $ lines i4
    let against = getVotes $ lines i5

    -- Initializating the population:
    initPop <- replicateM population $ initialization seats (take parties [0,0..])

    -- Looping begins.
    let loop 0 pop = (pure . reverse) $ sortOn (objFunction pro against) pop
        loop n pop = do
            -- Select the best parents
            --print $ pro
            --print $ against
            --print $ head pop
            let ordPop = reverse $ sortOn (objFunction pro against) pop
            let prob = probabilitiesDown population population 0.0
            -- Make children
            newPop <- replicateM (population-1) $ do
                gen <- newStdGen
                let [next1, next2] = take 2 $ randomRs (0.0 , last prob) gen
                let parent1 = randomParents prob next1 0
                let parent2' = randomParents prob next2 0
                let parent2 = if parent2' == parent1 then parent2' `div` 2 else parent2'
                let child = makeChild (ordPop !! parent1) (ordPop !! parent2)
                return $ fill (seats - sum child) child
    
            -- Mutation here.
            newPopMut <- mapM (mutation $ parties-2) newPop

            -- Next population: We also add the best parent here
            loop (n-1) (head ordPop : newPopMut)


    -- Output results.    
    best <- head <$> loop iters initPop
    return (best, objFunction pro against best, partiesN, lawsN)

        
    

---------------------------------------------------
-- Auxiliar functions for the genetic algorithm. --
---------------------------------------------------

-- Replace a value in a given position by another value.
replace :: Int -> a -> [a] -> [a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

-- Initialization of the input
initialization :: Int -> [Int] -> IO [Int]
initialization seats congress = do
    let max = length congress - 1
    gen <- newStdGen
    let pos = fst $ randomR (0,max) gen
    if seats > 30
    then initialization (seats - 30) (replace pos (congress !! pos + 30) congress)
    else pure (replace pos (congress !! pos + seats) congress)

-- The objective value function.
objFunction :: [[Int]] -> [[Int]] -> [Int] -> Int
objFunction pro aga sol = sum $ zipWith getVal pro aga
    where getVal _ []  = 1
          getVal ps as = if sum [sol !! p | p <- ps] - sum [sol !! a | a <- as] > 0
                         then 1
                         else 0

-- Get gaps between probabilites in a list:
-- 5 element list : [1.0, 1.8, 2.4, 2.8, 3.0]
-- Used for selecting the best parents with higher probability.
probabilitiesDown :: Int -> Int -> Float -> [Float]
probabilitiesDown 0 _ _ = []
probabilitiesDown m n f = newF : probabilitiesDown (m-1) n newF
    where newF = f + (fromIntegral m / fromIntegral n)

-- Select one of the parents at random
-- but need to be fed a weighted list
-- modified as the list that probabilitiesDown gives.
randomParents :: [Float] -> Float -> Int -> Int
randomParents [] _ pos = pos
randomParents (x:prob) f pos
    | x < f = randomParents prob f (pos+1)
    | otherwise = pos

-- Parents procreate.
makeChild :: [Int] -> [Int] -> [Int]
makeChild p1 p2 = map (`div` 2) $ zipWith (+) p1 p2

-- The parents can procreate and have a child with not
-- all the seats occupied, so this function fills
-- the seats left by selecting a random party
-- and giving them the seats missing (Politics!!!).
fill :: Int -> [Int] -> [Int]
fill n child = replace cPos (n + child !! cPos) child
    where cPos = fromJust $ elemIndex (foldl1' min child) child

----------------
-- Mutations. --
----------------

-- Mutation 1: Swap the seats of two parties.
-- 15% odds of mutation.
mutate1 :: Int -> [Int] -> IO [Int]
mutate1 (-1) xs = pure xs
mutate1 n xs = do
    gen <- newStdGen

    let next1 = head $ randomRs (0 :: Int, length xs - 1 :: Int) gen \\ [n]
    let rand1 = fst $ random gen
    let res1 = if rand1 < mutFactor1
               then replace n (xs !! next1) (replace next1 (xs !! n) xs)
               else xs
    
    mutate1 (n-1) res1

-- Mutation 2: a party absorbs all the seats of another one.
-- 10% odds of mutation.
mutate2 :: Int -> [Int] -> IO [Int]
mutate2 (-1) xs = pure xs
mutate2 n xs = do
    gen <- newStdGen

    let next2 = head $ randomRs (0 :: Int, length xs - 1:: Int) gen \\ [n]
    let rand2 = fst $ random gen
    let res2 = if rand2 < mutFactor2
               then replace n ((xs !! next2) + (xs !! n)) (replace next2 0 xs)
               else xs

    mutate2 (n-1) res2

-- Mutation 3: a party takes quite a lot of votes from 4 random parties.
-- If you think this mutation doesn't make sense, so does politics.
-- 10% odds of mutation.
mutate3 :: Int -> [Int] -> IO [Int]
mutate3 (-1) xs = pure xs
mutate3 n xs = do
    gen <- newStdGen

    let totalSeats = sum xs
    let dif = totalSeats `div` 20

    let next3 = take 4 $ nub $ randomRs (0 :: Int, length xs - 1 :: Int) gen \\ [n]
    let rand3 = fst $ random gen
    let res3 = if rand3 < mutFactor3
               then mutate3Aux n (head next3) dif 
                    (mutate3Aux n (next3 !! 1) dif 
                    (mutate3Aux n (next3 !! 2) dif
                    (mutate3Aux n (next3 !! 3) dif xs
                    )))
               else xs
    
    mutate3 (n-1) res3

-- Mutation 4: chooses one of the 3 mutations at random.
-- 10% odds of mutation.
mutate4 :: Int -> [Int] -> IO [Int]
mutate4 n xs = do
    gen <- newStdGen
    let mut = fst $ randomR (1 :: Int, 3 :: Int) gen
    if mut == 1 then mutate1 n xs
                else if mut == 2 then mutate2 n xs
                                 else mutate3 n xs

               
-- It output the substrction of 2 nums but if its a negative number it outputs 0 instead.
safeMinus :: Int -> Int -> Int
safeMinus x y = max 0 (x - y)

-- Auxiliar function for mutate3.
mutate3Aux :: Int -> Int -> Int -> [Int] -> [Int]
mutate3Aux pos1 pos2 dif xs = replace pos1 newVal1 (replace pos2 newVal2 xs)
    where val1 = xs !! pos1
          val2 = xs !! pos2
          newVal2 = safeMinus val2 dif
          newVal1 = newVal2 + val1
    



-------------------------------------------
-- Auxiliar functions for input reading. --
-------------------------------------------

-- Function to split in any list of elements provided in a list (We use it with Strings here).
splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl (\ys d -> ys >>= splitOn d) [xs] ds

-- Constant giving the dividing strings to read the input.
splits :: [String]
splits = [ "\n\nNumber of seats:\n"
         , "\n\nParties:\n"
         , "\n\nLaws:\n"
         , "\n\nPro:\n"
         , "\n\nAgainst:\n"
         ]

-- Function made to read who were against a law or in favour of a law from input.
getVotes :: [String] -> [[Int]]
getVotes xs = map choice xs
    where choice x = if length (words x) > 2 
                     then (map (read :: String -> Int) . splitOn "," . (!! 2) . words) x
                     else []

    