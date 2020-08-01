module HashFunctions where

import Data.Bits
import Data.String

import Types
import Examples

hash :: HashFun
hash x
  = op x''
  where
    op x = xor (shiftR x 16) x
    x'  = op x * 73244475
    x'' = op x' * 73244475


countOnes :: Int -> Int
countOnes dec
  | dec == 0             = 0
  | dec .&. (bit 0) == 1 = (1 + countOnes (shiftR dec 1))
  | otherwise            = countOnes (shiftR dec 1)


countOnesFrom :: Int -> Int -> Int
countOnesFrom index dec
  = countOnes dec2
  where
    mask = (bit index - 1)
    dec2 = dec .&. mask

getIndex :: Int -> Int ->Int -> Int
getIndex dec batch size
  = shiftR (maskShifted .&. dec) shiftSize
  where
    mask = (bit size -1)
    maskShifted = shiftL mask shiftSize
    shiftSize = batch * size

-- PRE: n is less than length of list
replace :: Int -> [a] -> a -> [a]
replace n list replacement
  = first ++ (replacement : (drop 1 second))
  where
    (first, second) = splitAt (n) list

insertAt :: Int -> a -> [a] -> [a]
insertAt n insertion list
  = first  ++ (insertion : second)
  where
    (first, second) = splitAt n list

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie func1 func2 (Leaf leaves)
  = func2 leaves
sumTrie func1 func2 (Node bitVector [])
  = 0
sumTrie func1 func2 (Node bitVector ((Term int) : subNodes))
  = func1 int + (sumTrie func1 func2 (Node bitVector subNodes))
sumTrie func1 func2 (Node bitVector ((SubTrie trie) : subNodes))
  = (sumTrie func1 func2 trie) + (sumTrie func1 func2 (Node bitVector subNodes))


member :: Int -> Hash -> Trie -> Int -> Bool
member value hashNo trie blockSize
  = member' value blockSize 0 hashed trie
  where
    hashed = hash hashNo


member' :: Int -> Int -> Int -> Int -> Trie -> Bool
member' value blockSize index hashed (Leaf [])
  = False
member' value blockSize index hashed (Node bitVector [])
  = False

member' value blockSize index hashed (Leaf (leaf : leaves))
  | leaf == value        = True
  | otherwise            = member' value blockSize index hashed (Leaf leaves)

member' value blockSize index hashed (Node bitVector (Term int : subNodes))
  | (bitVector == path)  = (int == value)
  | otherwise            = member' value blockSize index hashed (Node bitVector subNodes)
  where
    path = getIndex hashed index blockSize

member' value blockSize index hashed (Node bitVector ((SubTrie trie) : subNodes))
  | bitVector == path    = member' value blockSize (index+1) hashed trie
  | otherwise            = member' value blockSize index hashed (Node bitVector subNodes)
  where
    path = getIndex hashed index blockSize

checkElem :: Int -> [Int] -> Bool
checkElem value []
  = False
checkElem value (leaf : leaves)
  | value /= leaf = checkElem value leaves
  | otherwise     = True

addSubNode :: SubNode -> Trie -> Trie
addSubNode (subTrie) (Node bitVector subNodes)
   = Node bitVector (subTrie : subNodes)

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hashFun maxDepth blockSize value trie
  = insert' 0 trie
  where
    insert' :: Int -> Trie -> Trie
    insert' currDepth (Leaf leaves)
      | currDepth == maxDepth - 1 = Leaf [value]
      | checkElem value leaves    = Leaf leaves
      | otherwise                 = Leaf (value : leaves)
    insert' currDepth trie'@(Node bitVector subnodes@(subN : subNs))
      | currDepth == maxDepth - 1 = Leaf [value]
      | bitVector == 0            = Node 1 (insertAt 0 (Term value) subnodes)
      | bitVector == 1            = insert' (currDepth + 1) (Node bitVector subnodes)
    insert' currDepth trie'@(Node 1 ((SubTrie trie'') : subnodes))
      | currDepth == maxDepth - 1 = Leaf [value]
      | otherwise                 = addSubNode replacement (insert' currDepth (Node 1 subnodes))
      where
        replacement               = SubTrie (insert' currDepth trie'')
    insert' _ trie'
      = trie'

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hashFun maxDepth blockSize values
  = foldr (insert hashFun maxDepth blockSize 0 empty) values
