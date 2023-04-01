{-
    File: Main.hs
    Project: VUT-FIT FLP Functional project (knapsack-problem-solver)
    Author: Adam Švenk (xsvenk00)
    Version: 1.0
    Date: 02.04.2023
-}

{- 
		Import modules
-}
module Main (main) where
import GHC.Base ()
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile, hGetContents, hClose)
import GHC.IO.Handle.FD (stdin)
import GHC.IO.Handle.Types (Handle)
import Control.Monad ()
import System.Random (getStdGen, Random(randomRs, randomR), RandomGen(split), StdGen)
import Types (Item(cost, weight), Knapsack(..), Solution(..))
import ParseInput ( parseKnapSack ) 
import Text.ParserCombinators.Parsec (parse)
import Data.List (nub, maximumBy)
import Data.ByteString ()
import Data.Ord (comparing)

-- Tunable parameters
reproductionRate :: Double
reproductionRate = 0.3

crossoverRate :: Double
crossoverRate = 0.50

mutationRate :: Double
mutationRate = 0.1

iterations :: Int
iterations = 500

populationSize :: Int
populationSize = 1000
-- End of tunable parameters

-- Pick items from the knapsack based on the provided solution
buildItems :: Knapsack -> Solution -> [Item]
buildItems Knapsack{items = xs} Solution{included = bs} = [x | (x, b) <- zip xs bs, b]

-- Sum the cost of the items
itemsCost :: [Item] -> Int
itemsCost items = sum $ map cost items

-- Validate the provided solution
validateSolution :: Knapsack -> Solution -> Bool
validateSolution knapsack solution = itemsWeight (buildItems knapsack solution) <= maxWeight knapsack && itemsCost (buildItems knapsack solution) >= minCost knapsack
    where
        -- Sum the weight of the items
        itemsWeight :: [Item] -> Int
        itemsWeight items = sum $ map weight items

-- Get all possible solutions for the given knapsack (brute force)
getAllSolutions :: Knapsack -> [Solution]
getAllSolutions knapsack = filter (validateSolution knapsack) (boolCombinations (length (items knapsack)))
    where
        boolCombinations :: Int -> [Solution]
        boolCombinations 0 = [Solution []]
        boolCombinations n = [Solution (x:xs) | x <- [False, True], Solution xs <- boolCombinations (n-1)]

bestSolution :: Knapsack -> [Solution] -> String
bestSolution _ [] = "False"
bestSolution knapsack solutions = show $ head $ filter (\x -> itemsCost (buildItems knapsack x) == maximum (map (itemsCost . buildItems knapsack) solutions)) solutions

-- Generate a list of 4 integers that are unique
generateUniqueRandomIntegers :: Int -> StdGen -> ([Int], StdGen)
generateUniqueRandomIntegers maxNum gen = (take 4 $ nub $ randomRs (0, maxNum) gen, snd $ split gen)

-- Calculate the fitness of the provided solution for the knapsack problem
-- If the solution is not valid, return 0
checkFitness :: Knapsack -> Solution -> Int
checkFitness knapsack solution
    | validateSolution knapsack solution = itemsCost $ buildItems knapsack solution
    | otherwise = 0

-- Do the crossover for the provided solutions and create new solutions
crossover :: [Solution] -> [Solution]
crossover [] = []
crossover [s] = [s]
crossover (parent1:parent2:_) = [crossover' parent1 parent2, crossover' parent2 parent1]
    where
        crossover' :: Solution -> Solution -> Solution
        crossover' Solution {included = p1} Solution {included = p2} = Solution $ take midpoint p1 ++ drop midpoint p2
            where
            midpoint = length p1 `div` 2

-- Tournament selection based on the fitness of the solutions
tournament :: Knapsack -> Solution -> Solution -> Solution
tournament knapsack parent1 parent2
    | checkFitness knapsack parent1 > checkFitness knapsack parent2 = parent1
    | otherwise = parent2

-- Randomly select 4 solutions from the population and do the tournament selection
selection :: Knapsack -> [Solution] -> StdGen -> ([Solution], StdGen)
selection knapsack oldpop gen = ([parent1, parent2], newgen)
    where
        (randomIndexes, newgen) = generateUniqueRandomIntegers (length oldpop) gen
        population = shuffleSolutions oldpop (map (`mod` length oldpop) randomIndexes)
        parent1 = tournament knapsack (head population) (population !! 1)
        parent2 = tournament knapsack (population !! 2) (population !! 3)

-- Mutate single bit based on the mutation rate
mutateBit :: Bool -> StdGen -> (Bool, StdGen)
mutateBit inputBit gen
    | randomNumber < mutationRate = (not inputBit, newGen)
    | otherwise = (inputBit, snd $ split gen)
    where
        (randomNumber, newGen) = randomR (0.0, 1.0) gen

-- Mutate the provided single solution
mutate' :: Solution -> StdGen -> (Solution, StdGen)
mutate' Solution {included = solution} gen = (Solution $ mutate'' solution gen, snd $ split gen)
    where
        mutate'' :: [Bool] -> StdGen -> [Bool]
        mutate'' [] _ = []
        mutate'' (x:xs) gen' = let (newBit, newGen) = mutateBit x gen' in newBit : mutate'' xs newGen                

-- Mutate the provided solutions
mutate :: [Solution] -> StdGen -> ([Solution], StdGen)
mutate solutions gen = (mutatedSolutions, finalGen)
    where
        (mutatedSolutions, finalGen) = foldr mutateAccumulator ([], gen) solutions
        mutateAccumulator solution (acc, g) = let (mutatedSolution, newGen) = mutate' solution g in (mutatedSolution : acc, newGen) 

-- Select 4 solutions based on the provided list of 4 randomly generated indexes
shuffleSolutions :: [Solution] -> [Int] -> [Solution]
shuffleSolutions solutions = map ((solutions !!) . (`mod` length solutions))

-- Create a first population of random solutions
createRandomSolutions :: Int -> Int -> StdGen -> ([Solution], StdGen)
createRandomSolutions numSolutions len gen = createRandomSolution' numSolutions len gen []
  where
    createRandomSolution' 0 _ g solutions = (solutions, g)
    createRandomSolution' n l g solutions =
      let (solution, newGen) = createRandomSolution'' l g
      in createRandomSolution' (n-1) l newGen (solution:solutions)

    createRandomSolution'' len' gen' =
      let solution = Solution (take len' $ randomRs (True, False) gen')
          newGen = snd $ split gen'
      in (solution, newGen)

nextGen' :: [Solution] -> [Bool] -> StdGen -> ([Solution], StdGen)
nextGen' population [True, _, _] gen = (population, gen)
nextGen' _ [False, False, False] gen = ([], gen)
nextGen' _ [False, False, True] gen = ([], gen)
nextGen' population [False, True, False] gen = (crossover population, gen)
nextGen' population [False, True, True] gen = mutate (crossover population) gen
nextGen' _ _ gen = ([], gen)

solveGenetic :: Knapsack -> [Solution] -> Int -> StdGen -> Solution
solveGenetic knapsack population currIter gen
    | currIter == iterations = maximumBy (comparing $ checkFitness knapsack) population
    | currIter == 0 = solveGenetic knapsack firstPopulation (currIter + 1) newGen
    | otherwise = solveGenetic knapsack newPopulation (currIter + 1) newGen'
        where
            (firstPopulation, newGen) = createRandomSolutions populationSize (length (items knapsack)) gen
            (newPopulation, newGen') = nextGen knapsack population [] gen

nextGen :: Knapsack -> [Solution] -> [Solution] -> StdGen -> ([Solution], StdGen)
nextGen knapsack population newPop gen =
    if length newPop == length population
    then (newPop, snd $ split gen)
    else
        let (r:c:m:_) = take 3 $ randomRs (0.00, 1.00) gen
            bools = [r < reproductionRate, c < crossoverRate, m < mutationRate] -- pass the Doubles to createBools
            (selected, g1) = selection knapsack population (snd $ split gen)
            (newSols, g2) = nextGen' selected bools g1 -- pass the bools to nextGen'
        in nextGen knapsack population (newPop ++ newSols) g2

validate :: Knapsack -> Solution -> String
validate knapsack solution = if validateSolution knapsack solution then show solution else "False"

solveKnapsack :: Knapsack -> String -> StdGen -> String
solveKnapsack knapsack solverType gen
    | solverType == "-i" = show knapsack
    | solverType == "-b" = bestSolution knapsack $ getAllSolutions knapsack
    | solverType == "-o" = validate knapsack (solveGenetic knapsack [] 0 gen)
    | otherwise = error "Error: Invalid solver type."

-- Parse the program arguments
parseArgs :: [String] -> IO GHC.IO.Handle.Types.Handle
parseArgs["-i"] = return GHC.IO.Handle.FD.stdin
parseArgs["-b"] = return GHC.IO.Handle.FD.stdin
parseArgs["-o"] = return GHC.IO.Handle.FD.stdin
parseArgs["-i", fileName] = openFile fileName ReadMode
parseArgs["-b", fileName] = openFile fileName ReadMode
parseArgs["-o", fileName] = openFile fileName ReadMode
parseArgs _ = error "Error: Invalid program arguments."

-- Main function
main :: IO ()
main = do
    args <- getArgs
    gen <- getStdGen
    handle <- parseArgs args
    contents <- hGetContents handle

    case parse parseKnapSack "" contents of
        Left err -> putStrLn $ "Error: " ++ show err
        Right knapsack -> putStrLn $ solveKnapsack knapsack (head args) gen

    hClose handle
