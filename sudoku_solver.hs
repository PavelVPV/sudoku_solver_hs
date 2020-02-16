{-- |
Copyright (c) 2020, PavelVPV
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-- |
Module      :  Sudoku solver
Description :  The module provides a set of functions that are used to solve Sudoku puzzle using Algorithm X
Copyright   :  (c) PavelVPV
License     :  BSD-3-Clause

Maintainer  :  rokr.vpv@gmail.com
Stability   :  experimental
Portability :  portable

Brief description:
- Use `dryRun <number of cells to remove>` to generate and solve a sudoku puzzle.
- Use `generateSudoku` to generate a complete (solved) Sudoku.
- Use `removeCells <number of cells to remove>` to remove the provided number of cells from a complete sudoku.
- Use `generateExactCover <sudoku puzzle>` to generate an exact cover matrix for a sudoku puzzle, which will be used by Algorithm X to solve the Sudoku puzzle.
- Use `algorithmX <exact cover matrix>` to find a solution of a sudoku puzzle.
- Use `extractSolution` to covert a solution to a sudoku matrix.
- Use `printSudoku` to print a sudoku puzzle.

See `dryRun` function for details.
-}

{-# LANGUAGE ExtendedDefaultRules #-}

import System.Random
import Data.List.Extra
import Text.Printf
import Data.Maybe
import Control.Exception
import Control.Monad

sudokuBlockSize size = round (sqrt (fromIntegral size))

-- |Base matrix used for sudoku generation.
baseMatrix = [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,3,4,5,6,7,8,9,1],[5,6,7,8,9,1,2,3,4],[8,9,1,2,3,4,5,6,7],[3,4,5,6,7,8,9,1,2],[6,7,8,9,1,2,3,4,5],[9,1,2,3,4,5,6,7,8]]

-- |Takes last 'n' last elements from the list
takeLast :: (Num b, Eq b) => b -> [a] -> [a]
takeLast 0 l = []
takeLast 1 l = [last l]
takeLast n l = (takeLast (n-1) (init l)) ++ [(last l)]

{-|
  Area transformation function set.
  Area contains 3 columns of the sudokue matrix.
-}

-- |Inverts first and last rows in the area of a sudoku matrix.
invertArea :: [[a]] -> [[a]]
invertArea (a:b:c:rest) = c:b:a:rest

-- |Shifts to the right columns in the area of a sudoku matrix.
shiftRightArea :: [[a]] -> [[a]]
shiftRightArea (a:b:c:rest) = c:a:b:rest

-- |Shifts to the left columns in the area of a sudoku matrix.
shiftLeftArea :: [[a]] -> [[a]]
shiftLeftArea (a:b:c:rest) = b:c:a:rest

-- |Defines list of area transformation functions.
transformArea = [(invertArea), (shiftRightArea), (shiftLeftArea)]
          
{-|
  Matrix transformation function set.
-}

-- |Inverts first and last areas in a sudoku matrix.
invertMatrix :: [[a]] -> [[a]]
invertMatrix m = m3 ++ m2 ++ m1
    where m1 = take 3 m
          m2 = take 3 (drop 3 m)
          m3 = takeLast 3 m

-- |Shifts to the right areas in a sudoku matrix.
shiftRightMatrix :: [[a]] -> [[a]]
shiftRightMatrix m = m3 ++ m1 ++ m2
    where m1 = take 3 m
          m2 = take 3 (drop 3 m)
          m3 = takeLast 3 m
          
-- |Shifts to the left areas in a sudoku matrix.
shiftLeftMatrix :: [[a]] -> [[a]]
shiftLeftMatrix m = m3 ++ m1 ++ m2
    where m1 = take 3 m
          m2 = take 3 (drop 3 m)
          m3 = takeLast 3 m
          
-- |Defines list of matrix transformation functions.
transformMatrix = [invertMatrix, shiftRightMatrix, shiftLeftMatrix]
    
-- |Translates rows to columns.
rowToColumns_ :: [a] -> [[a]] -> [[a]]
rowToColumns_ [] c = c
rowToColumns_ x [] = [x]
rowToColumns_ [x] [c] = [c ++ [x]]
rowToColumns_ (x:xs) (c:cs) = [c ++ [x]] ++ (rowToColumns_ xs cs)

-- |Helper function for translating each row to column in matrix.
forEachRow :: [[a]] -> [[a]] -> [[a]]
forEachRow [] c = c
forEachRow m [] = forEachRow (tail m) (rowToColumns_ (head m) ([[] | c <- [1..columnLength]]))
    where columnLength = length (m !! 0)
forEachRow (x:xs) c = forEachRow xs (rowToColumns_ x c)

-- |Translates rows to columns.
-- This allows to apply the same tranformation functions to columns as for rows.
rowsToColumns :: [[a]] -> [[a]]
rowsToColumns m = forEachRow m []

{-|
  Function set for generation a sudoku matrix.
-}

-- |Defines list of all transformation functions that can be applied to a Sudoku matrix.
shuffleList = [rowsToColumns] ++ transformMatrix ++ transformArea

generateSudoku_  :: (Eq t1, Num t1, RandomGen t2) => t1 -> t2 -> [[a]] -> ([[a]], t2)
generateSudoku_ 0 g m = (m, g)
generateSudoku_ n g m = generateSudoku_ (n - 1) newG ((shuffleList !! r) m)
    where (r,newG) = randomR (0, (length(shuffleList) - 1)) g

-- |Generates a complete sudoku matrix.
generateSudoku :: (RandomGen r) => r -> ([[Int]], r)
generateSudoku g = do
    let (r,ng) = randomR (30 :: Int, 100 :: Int) g
    (generateSudoku_ r ng baseMatrix)

-- |Replaces `i` element in the `r` row with `0`.
replaceElem r i = [y | x <- [0..((length r) - 1)], let y = if x /= i then r !! x else 0]

-- |Removes `num` cells from a sudoku matrix by replacing them with `0`.
-- Takes a sudoke matrix, number of cells to remove and random number generator.
removeCells :: (RandomGen g) => [[Int]] -> Int -> g -> ([[Int]], g)
removeCells matrix 0 g = (matrix, g)
removeCells matrix num g = do
  let (random,ng) = randomR (0 :: Int, 80 :: Int) g 
  let row = fromIntegral $ random `div` 9
  let column = fromIntegral $ random - (row * 9)
  let newMatrix = [y | x <- [0..((length matrix) - 1)], let y = if x /= row then matrix !! x else replaceElem (matrix !! x) column]
  removeCells newMatrix (num - 1) ng

{-|
  Function set for Algorithm X.
-}

-- |Summarizes number of non-zero elements in a row
sumOfElems :: [Int] -> Int
sumOfElems row = foldl (\acc x -> if x /= 0 then (acc + 1) else acc) 0 row

-- |Finds a set of columns with minimum number of non-zero elements.
-- Step 2 of the Algorthim X.
-- Takes as an argument an exact cover matrix.
-- Returns either set of columns or Nothing.
findMinColumns :: [[Int]] -> Maybe [[Int]]
findMinColumns [] = Nothing
findMinColumns matrix 
  | (length out) == 0 = Nothing
  | otherwise = Just out
  where columnSums = foldl (\acc x -> zipWith (\a b -> if b /= 0 then a + 1 else a) acc x) [0 | x <- [1..(length (matrix !! 0))]] matrix
        minValue = minimum columnSums
        out = [(getMatrixColumn matrix x) | x <- [0..(length(matrix !! 0) - 1)], (columnSums !! x) == minValue]

-- |Finds a set of rows, which have non-zero elements.
-- Step 3 of the AlgorithmX.
-- Takes as an argument a column.
-- Returns a list of row indexes.
findFirstRows :: [Int] -> Maybe [Int]
findFirstRows [] = Nothing
findFirstRows column
  | length out == 0 = Nothing
  | otherwise = Just out
  where out = [x | x <- [0..((length column) - 1)], (column !! x) /= 0]

-- |Helper function to remove a row from an exact cover matrix.
removeRow_ :: [a] -> Int -> Int -> [a]
removeRow_ []     rowIdToRemove currentRowId = []
removeRow_ (x:xs) rowIdToRemove currentRowId
  | currentRowId == rowIdToRemove = xs
  | otherwise = [x] ++ removeRow_ xs rowIdToRemove (currentRowId + 1)

-- |Removes row from an exact cover matrix by its ID.  
removeRow matrix rowIdToRemove = removeRow_ matrix rowIdToRemove 0

-- |Helper function to remove a list of rows from an exact cover matrix. 
removeRows_ :: [[Int]] -> Int -> [Int] -> [Int] -> ([[Int]], [Int])
removeRows_ []     currentRowId r      idx = ([], [])
removeRows_ matrix currentRowId []     idx = (matrix, idx)
removeRows_ matrix currentRowId (r:rs) idx
  | r /= 0 = removeRows_ (removeRow matrix currentRowId) currentRowId rs (removeRow idx currentRowId)
  | otherwise = removeRows_ matrix (currentRowId + 1) rs idx

-- |Removes rows from an exact cover matrix by a list of their IDs.
-- Step 5.2 of the Algorithm X.
removeRows matrix column idx = removeRows_ matrix 0 column idx

-- |Remove a column from an exact cover matrix by its ID.
-- Step 5.3 of the Algorithm X.
-- If there is only one column in the matrix, it will result in [[]]. 
-- To avoid that, `filter` is applied.
removeColumn matrix j = filter (not . null) $ foldl (\acc x -> acc ++ [removeRow x j]) [] matrix

-- |Returns a column from an exact cover matrix.
-- Step 5.1 of the Algorithm X.
getMatrixColumn :: [[Int]] -> Int -> [Int]
getMatrixColumn matrix c = foldl (\acc x -> acc ++ [x !! c]) [] matrix

-- |Reduces an exact cover matrix
-- Step 5 of the Algorithm X.
reduceMatrix :: [[Int]] -> [Int] -> Int -> [Int] -> ([[Int]], [Int])
reduceMatrix [] idx j c = ([], [])
reduceMatrix matrix idx j [] = (matrix, idx)
reduceMatrix matrix idx j (c:cs)
  | c /= 0 = reduceMatrix (removeColumn m2 j) newIdx j cs
  | otherwise = reduceMatrix matrix idx (j+1) cs
  where (m2, newIdx) = removeRows matrix (getMatrixColumn matrix j) idx

-- |Maps a 3 arg tuple to a function.
mapTuple :: (Monad m) => (a -> b -> c -> m d) -> (a, b, c) -> m d
mapTuple f (a, b, c) = (f a b c)

-- |Removes duplicates from the list
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                     then seen
                                     else seen ++ [x]) []

-- |Finds first non-Nothing element in a list
findFirstSolution :: (Eq b) => (a -> Maybe b) -> [a] -> Maybe b
findFirstSolution f [] = Nothing
findFirstSolution f (x:xs) 
  | solution == Nothing = findFirstSolution f xs
  | otherwise = solution
  where solution = f x

-- Algorithm X
--1. If the matrix A has no columns, the current partial solution is a valid solution; terminate successfully.
--2. Otherwise choose a column c (deterministically).
--3. Choose a row r such that Ar, c = 1 (nondeterministically).
--4. Include row r in the partial solution.
--5. For each column j such that Ar, j = 1,
--     1. for each row i such that Ai, j = 1,
--     2.   delete row i from matrix A.
--     3. delete column j from matrix A.
--6. Repeat this algorithm recursively on the reduced matrix A.

-- |Applies Algorithm X to an exact cover matrix.
algorithmX matrix = algorithmX_ matrix [0..(length(matrix) - 1)] []

-- Params: matrix, row IDs, solution
-- Returns list of rows from an exact cover matrix or Nothing
algorithmX_ :: [[Int]] -> [Int] -> [Int] -> Maybe [Int]
algorithmX_ [] idx out = Just out
algorithmX_ matrix idx out = do
  -- Step 2 (NOTE: Takes list of columns, not one column)
  columnCs <- findMinColumns matrix
  -- Step 3 (NOTE: Takes list of rows, not one row)
--  rowIds <- sequence $ map (findFirstRows) columnCs
  rowIds <- sequence $ foldl (\acc x -> acc ++ [findFirstRows x]) [] columnCs
  -- Some combinations of Ar,c == 1 may give same rows, duplicates need to be removed
  -- Flatten the list [[],[]] -> []
  let flattenRowIds = concat $ removeDuplicates $ rowIds
  -- Extract rows using their IDs
--  let rowRs = map (matrix !!) flattenRowIds
  let rowRs = foldl (\acc x -> acc ++ [(matrix !! x)]) [] flattenRowIds
  -- Step 4: Compose list of solutions
  let newOut = map ((snoc) out) (map (idx !!) flattenRowIds)
  -- Step 5: Reduce the matrix. reducedMatrix is a tuple of matrix and left row IDs
--  let reducedMatrix = map (reduceMatrix matrix idx 0) rowRs
  let reducedMatrix = foldl (\acc x -> acc ++ [(reduceMatrix matrix idx 0 x)]) [] rowRs
  -- Zip matrices with assotiated current solutions
  let zippedParams = zipWith (\c (a, b) -> (a, b, c)) newOut reducedMatrix
  -- Step 6: Go through all reduced matrices to find first solution
  findFirstSolution (mapTuple (algorithmX_)) zippedParams

-- |Generates an exact cover matrix for a sudoku
generateExactCover sudoku = [genRow sudoku r c n | r <- [1..9], c <- [1..9], n <- [1..9]]
  where genRow sudoku r c n = cA ++ cB ++ cC ++ cD
          where cA = [if (x == ((r - 1) * 9 + c)) && ((n == sudokuNumber) || (sudokuNumber == 0)) then n else 0 | x <- [1..81]]
                cB = [if (x == ((r - 1) * 9 + n)) && ((n == sudokuNumber) || (sudokuNumber == 0)) then n else 0 | x <- [1..81]]
                cC = [if (x == ((c - 1) * 9 + n)) && ((n == sudokuNumber) || (sudokuNumber == 0)) then n else 0 | x <- [1..81]]
                cD = [if (x == ((q - 1) * 9 + n)) && ((n == sudokuNumber) || (sudokuNumber == 0)) then n else 0 | x <- [1..81]]
                q = ((((r - 1) `div` 3) * 3) + ((c - 1) `div` 3) + 1)
                sudokuNumber = (sudoku !! (r - 1)) !! (c - 1)

-- |Extracts a complete sudoku from a solution
extractSolution solution = extractSolution_ solution []
  where extractSolution_ [] acc = acc
        extractSolution_ r acc = extractSolution_ r2 (acc ++ [row])
          where (r2, row) = extractRow r [] 0
                extractRow x acc 9 = (x,acc)
                extractRow (x:xs) acc i = extractRow xs (acc ++ [n]) (i + 1)
                  where n = extractNumber 9 x
                        extractNumber sz result = (n + 1)
                          where (ceil, rem) = properFraction ((realToFrac result) / (realToFrac sz))
                                n = round (rem * (realToFrac sz))

{-|
  A function set for printing Sudoku.
-}

-- |Returns a separator string.
makeSeparator n = concat $ [a ++ b | x <- [1..(n*n)], 
                  let a = if x /= (n*n) then "--" else "-",
                  let b = if x `rem` n == 0 && x /= (n*n) then "+-" else ""]
                  
-- |Prints a sudoku row.
printSudokuRow :: [Int] -> Int -> Int -> IO ()
printSudokuRow [] size i = printf "\n"
printSudokuRow (r:rs) size i = do
  if r /= 0 then printf "%d " r else printf "  "
  if (i `rem` block_size == 0 && i /= size) then (printf "| ") else return ()
  printSudokuRow rs size (i + 1)
  where block_size = (round (sqrt (fromIntegral size)))

-- |Prints a sudoku matrix.
-- Can print both a complete sudoku matrix and the one with removed cells.
printSudoku :: [[Int]] -> Int -> Int -> IO ()
printSudoku [] size i = printf "\n"
printSudoku (m:ms) size i = do
  printSudokuRow m size 1  
  if ((i `rem` block_size == 0) && (i /= size)) then (printf "%s\n" (makeSeparator block_size)) else return ()
  printSudoku ms size (i + 1)
  where block_size = (round (sqrt (fromIntegral size)))
  
{-|
  Other functions.
-}

-- |Dry run function that generates a Sudoku puzzle, removes provided number of cells, solves the generated sudoku.
dryRun :: Int -> IO ()
dryRun cellsToRemove = do
  g <- newStdGen
  let (sudoku, nextG) = generateSudoku g
  putStrLn "Complete Sudoku:"
  printSudoku sudoku 9 1
  let (emptySudoku, _) = removeCells sudoku cellsToRemove nextG
  putStrLn "Unsolved Sudoku:"
  printSudoku emptySudoku 9 1
  let exactCoverSudoku = generateExactCover emptySudoku
  putStrLn "Searching for a solution...\n"
  case algorithmX exactCoverSudoku of
    Nothing -> putStrLn "Sorry, I'm failed to solve sudoku :("
    Just a -> do
      putStrLn "Result:"
      -- Sort result to print a sudoku matrix correctly
      printSudoku (extractSolution . sort $ a) 9 1

{-|
  Tests.
-}

testMatrix1 = [[0,0,1,0],[1,0,0,1],[0,1,1,0],[1,0,0,1]]
testMatrix2 = [[1,0,0,0,1,0,0,0,1,0,0,0],[1,0,0,0,0,0,1,0,0,0,1,0],[0,1,0,0,0,1,0,0,1,0,0,0],[0,1,0,0,0,0,0,1,0,0,1,0],[0,0,1,0,1,0,0,0,0,1,0,0],[0,0,1,0,0,0,1,0,0,0,0,1],[0,0,0,1,0,1,0,0,0,1,0,0],[0,0,0,1,0,0,0,1,0,0,0,1]]
testMatrix3 = [[1,0,0,1,0,0,1],[1,0,0,1,0,0,0],[0,0,0,1,1,0,1],[0,0,1,0,1,1,0],[0,1,1,0,0,1,1],[0,1,0,0,0,0,1]]
solutionMatrix1 = [2,1]
solutionMatrix2 = [0,3,5,6]
solutionMatrix3 = [1,3,5]

testVector = [testMatrix1, testMatrix2, testMatrix3]
solutionVector = [solutionMatrix1, solutionMatrix2, solutionMatrix3]

doTest [] [] = True
doTest (x:xs) (y:ys) = 
  case algorithmX x of
    Just solution -> assert (solution == y) (doTest xs ys)
    Nothing -> assert ([] == y) (doTest xs ys)

tests :: IO ()
tests = do 
  case doTest testVector solutionVector of
    True -> putStrLn "Tests passed"
    False -> putStrLn "Tests failed"
