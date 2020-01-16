-- Shows a board of queens.
-- The board is represented by a list of ints with length N, containing values 1..N that
-- represents the column of a queen at the row corresponding to the index of the value in the list.
-- For instance, [1,2,3,4] represents a 4x4 board with all queens placed on the diagonal.
-- This representation of the board only allows one queen per row, but since all solutions to the
-- n-queens problem will have one queen per row, this is not a limitation.
showBoard cs = unlines $ map showRow cs
    where 
        showRow c = take (c - 1) dots ++ "Q" ++ take (size - c) dots
        size = length cs
        dots = repeat '-'

-- Determines if a Queen placed at row qr and column qc checks the square at r,c
checks :: Int -> Int -> Int -> Int -> Bool
checks qr qc r c = qc == c || (r-qr) == abs(c-qc)

-- Computes all solutions to the n-queens problem. See https://en.wikipedia.org/wiki/Eight_queens_puzzle
queens :: Int -> [[Int]]
queens n = subQueens 1 (\r c -> False)
    where
        subQueens qr checked
            | qr > n = [[]]
            | otherwise = [ qc : rest | qc <- filter (not . (checked qr)) [1..n], rest <- subQueens (qr + 1) $ both checked (checks qr qc)]
        both ch1 ch2 r c = (ch1 r c) || (ch2 r c)