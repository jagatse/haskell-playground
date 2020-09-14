
-- Solutions to problems in: https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems 
-- Used as exercises when reading: http://learnyouahaskell.com/

-- Problem 01
-- myLast -- Get the last element of a list.
myLast [x] = x
myLast (x:xs) = myLast xs
myLast _ = error "Attempt to get the last element of an empty list"

-- Problem 02
-- myButLast -- Find the last but one element of a list.
myButLast [x, l] = x
myButLast (x:xs) = myButLast xs

-- Problem 03
-- elementAt -- Find the K'th element of a list. The first element in the list is number 1.
elementAt (x:xs) 1 = x
elementAt (x:xs) n 
    | n > 1 = elementAt xs (n-1)
    | True = error $ "Attempt to use an index lower than 1: " ++ show n

-- Problem 04
-- myLength -- Find the number of elements of a list.
myLength :: (Num b) => [a] -> b -- Inferred type is: [a] -> Integer
myLength = len 0
    where 
        len n [] = n
        len n (x:xs) = len (n+1) xs

myLength_foldr :: (Foldable t, Num b) => t a -> b
myLength_foldr = foldr (\_ -> (+1)) 0

-- Problem 05
-- myReverse -- Reverse a list.
myReverse = rev []
    where
        rev rs [] = rs
        rev rs (x:xs) = rev (x:rs) xs

-- Problem 06
-- isPalindrome -- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome xs = xs == myReverse xs

-- Problem 07
-- flatten -- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List ls) = foldr (++) [] (map flatten ls)

-- Problem 08
-- compress -- Eliminate consecutive duplicates of list elements.
compress [] = []
compress (x:xs) = x : compress (dropWhile (x==) xs)

-- Problem 09
-- pack -- Pack consecutive duplicates of list elements into sub lists. If a list contains repeated elements they should be placed in separate sub lists.
pack [] = []
pack (x:xs) = (takeWhile (x==) (x:xs)) : pack (dropWhile (x==) xs)

-- Problem 10
-- encode -- Run-length encoding of a list.
-- Had to add an explicit type signature here to avoid: https://wiki.haskell.org/Monomorphism_restriction
encode ::  Eq b => [b] -> [(Int, b)] 
encode = (map (\l -> (length l, head l))) . pack


-- Problem 11
-- encodeModified -- Modified run-length encoding.
data Rle a = Single a | Multiple Int a deriving Show

encodeModified ::  Eq b => [b] -> [Rle b] 
encodeModified = (map (\l -> if length l == 1 then Single (head l) else Multiple (length l) (head l))) . pack
encodeModified' ls = map toRle $ encode ls
    where
        toRle (1, v) = Single v
        toRle (n, v) = Multiple n v


-- Problem 12
-- decodeModified -- Decode a run-length encoded list.
decodeModified [] = []
decodeModified ((Single x) : tokens) = x : decodeModified tokens
decodeModified ((Multiple 1 x) : tokens) = x : decodeModified tokens
decodeModified ((Multiple n x) : tokens) = x : decodeModified ((Multiple (n - 1) x) : tokens)

decodeModified' = foldr (++) [] . map tolist
    where
        tolist (Single x) = [x]
        tolist (Multiple n x) = take n $ repeat x


-- Problem 13
-- encodeDirect -- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them.
encodeDirect [] = []
encodeDirect (x:xs) = enc x 1 xs
    where
        enc y n (z:zs)
            | y == z = enc y (n+1) zs
        enc y 1 zs = (Single y) : encodeDirect zs 
        enc y n zs = (Multiple n y) : encodeDirect zs


-- Problem 14
-- dupli -- Duplicate the elements of a list.
dupli [] = []
dupli (x:xs) = x : x : dupli xs


-- Problem 15
-- repli -- Replicate the elements of a list a given number of times.
repli ls n = foldr (++) [] $ map (take n . repeat) ls 


-- Problem 16
-- dropEvery -- Drop every N'th element from a list.
dropEvery ls n = map fst $ filter ((/= 0) . snd) $ zip ls $ map ( `mod` n) [1..]


-- Problem 17
-- split -- Split a list into two parts; the length of the first part is given. Do not use any predefined predicates.
split ls n = (takeN n ls, dropN n ls)
    where
        takeN c ls 
            | c < 1 || ls == [] = []
        takeN c (l:ls) = l : takeN (c-1) ls
        dropN c ls
            | c < 1 || ls == [] = ls
        dropN c (l:ls) = dropN (c-1) ls


-- Problem 18
-- slice -- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice ls i k = take (k - i + 1) $ drop (i - 1) ls


-- Problem 19
-- rotate -- Rotate a list N places to the left. Hint: Use the predefined functions length and (++).
rotate ls n = (drop steps ls) ++ (take steps ls)
    where 
        steps = n `mod` (length ls) -- In Haskell, -1 `mod 5 == 4 
