

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
myLength = len 0
    where 
        len n [] = n
        len n (x:xs) = len (n+1) xs

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
encode ::  Eq b => [b] -> [(Int, b)]
encode = (map (\l -> (length l, head l))) . pack
