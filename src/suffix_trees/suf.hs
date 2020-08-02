data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] str
  = True
isPrefix str []
  = False
isPrefix str@(char : chars) (char' : chars')
  | char == char'       = isPrefix chars chars'
  | otherwise           = False

removePrefix :: String -> String -> String
removePrefix [] str
  = str
removePrefix (char : chars) (char' : chars')
--Pre: s is a prefix of s'
  = removePrefix chars chars'

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes str
  = [str] ++ suffixes (tail str)

isSubstring :: String -> String -> Bool
isSubstring str str'
  = foldl (||) False isSubstrings
  where
    isSubstrings = map (isPrefix str) (suffixes str')

findSubstrings :: String -> String -> [Int]
findSubstrings str str'
  = [] ++ (findSubstrings' 0 suff)
  where
    suff = suffixes str'
    findSubstrings' :: Int -> [String] -> [Int]
    findSubstrings' index []
      = []
    findSubstrings' index (suff : suffs)
      | isPrefix str suff     = [index] ++ findSubstrings' (index + 1) suffs
      | otherwise             = findSubstrings' (index + 1) suffs


------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf leaf)
  = [leaf]
getIndices (Node [])
  = []
getIndices (Node ((str, suffixTree) : item))
  = (getIndices suffixTree) ++ (getIndices (Node item))

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition str@(char : chars) str'@(char' : chars')
  | char == char'     = ((char : next1), next2, next3)
  where
    (next1, next2, next3) = partition chars chars'
partition str str'
  = ([], str, str')

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' str (Leaf i)
  | str == ""               = [i]
  | otherwise               = []
findSubstrings' str (Node [])
  = []
findSubstrings' str (Node ((str', suffixTree) : item))
  | rest1 == ""            = (getIndices suffixTree) ++ next
  | rest2 == ""            = (findSubstrings' rest1 suffixTree) ++ next
  | otherwise              = next
  where
    (common, rest1, rest2) = partition str str'
    next                   = (findSubstrings' str (Node item))

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (str, index) (Node subTree@((str', suffixTree) : item))
  | common == ""           = insert (str, index) (Node item)
  | common == str'         = insert (rest1, index) suffixTree
  | common != str'         = Node ((str', replacement) : item)
  where
    replacement = Node [(rest1, Leaf index), (rest2, suffixTree)]
    (common, rest1, rest2) = partition str str'
insert (str, index) (Node [])
  = Node [(s, Leaf index)]
-- This function is given
buildTree :: String -> SuffixTree
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1
  = "banana"

s2 :: String
s2
  = "mississippi"

t1 :: SuffixTree
t1
  = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2
  = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]
