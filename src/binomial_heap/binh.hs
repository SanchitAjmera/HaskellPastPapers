type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

value :: BinTree a -> a
value (Node a _ _)
  = a

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c)
  = c

combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees binTree@(Node root rank children) binTree'@(Node root' rank' children')
  | root < root' = Node root (rank + 1) (binTree' : children)
  | otherwise    = Node root' (rank' + 1) (binTree : children')
--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin binTrees
  = minimum (map value binTrees)

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h
  =  h
mergeHeaps h []
  = h
mergeHeaps binHeap@(binTree : binTrees) binHeap'@(binTree' : binTrees')
  | r < r'                   = binTree : (mergeHeaps binTrees binHeap')
  | r > r'                   = binTree' : (mergeHeaps binHeap binTrees')
  | r == r'                  = mergeHeaps [combineTrees binTree binTree'] (mergeHeaps binTrees binTrees')
  where
    r = rank binTree
    r' = rank binTree'

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert value binHeap
  = mergeHeaps [Node value 0 []] binHeap

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin binTrees
  = mergeHeaps [binTree | binTree <- binTrees , binTree /= minTree] minChildren
  where
    minValue = extractMin binTrees
    minTree  = [binTree | binTree <- binTrees, (value binTree == minValue)]!!0
    minChildren = reverse $ children minTree


remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

constructHeap :: Ord a => [a] -> BinHeap a -> BinHeap a
constructHeap [] binHeap
  = binHeap
constructHeap (item : items) binHeap
  = constructHeap items (insert item binHeap)

sortItem :: Ord a => BinHeap a -> [a]
sortItem []
  = []
sortItem binHeap
  = minValue : (sortItem $ deleteMin binHeap)
  where
    minValue = extractMin binHeap

binSort :: Ord a => [a] -> [a]
binSort items
  = sorted
  where
    binHeap = constructHeap items []
    sorted  = sortItem binHeap


--------------------------------------------------------------
-- PART III
binary :: Int -> [Int]
binary 0
  = []
binary decimal
  = (binary $ div decimal 2 ) ++ [mod decimal 2]


toBinary :: BinHeap a -> [Int]
toBinary binHeap
  = reverse $ toBinary' 0 binHeap
  where
    toBinary' :: Int -> BinHeap a -> [Int]
    toBinary' _ []
      = []
    toBinary' index binHeap'@(binTree : binTrees)
      | index == (rank binTree) = 1 : (toBinary' (index + 1) binTrees)
      | otherwise               = 0 : (toBinary' (index + 1) binHeap')

binarySum :: [Int] -> [Int] -> [Int]
binarySum bin bin'
  = binary decimalSum
  where
    decimalSum = (toDecimal bin (length bin - 1)) + (toDecimal bin' (length bin' - 1))
    toDecimal :: [Int] -> Int -> Int
    toDecimal [] _
      = 0
    toDecimal (b : bin) index
      | b == 1    = 2^index + toDecimal bin (index - 1)
      | otherwise = toDecimal bin (index - 1)


------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []],
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []],
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]
