module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp e table
  = fromJust $ lookup e table


-- 3 marks
vars :: Formula -> [Id]
vars (Var i)
  = [i]
vars (Not f)
  = sort $ nub $ vars f
vars (And f f')
  = sort $ nub $ (vars f) ++ (vars f')
vars (Or f f')
  = sort $ nub $ (vars f) ++ (vars f')


-- 1 mark
idMap :: Formula -> IdMap
idMap f
  = zip vs [1..(length vs)]
  where
    vs = vars f

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (Not (And f f'))
  = Or (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Or f f'))
  = And (toNNF (Not f)) (toNNF (Not f'))
toNNF (Not (Not f))
  = toNNF f
toNNF (And f f')
  = (And (toNNF f) (toNNF f'))
toNNF (Or f f')
  = (Or (toNNF f) (toNNF f'))
toNNF f
  = f

-- 3 marks
toCNF :: Formula -> CNF
toCNF f
  = toCNF' $ toNNF f
  where
    toCNF' :: NNF -> CNF
    toCNF' (Or cnf@(And f f') f'')
      = distribute cnf f''
    toCNF' (Or f cnf@(And f' f''))
      = distribute cnf f
    toCNF' (And f f')
      = And (toCNF f) (toCNF f')
    toCNF' (Or f f')
      = Or (toCNF f) (toCNF f')
    toCNF' f
      = f

-- 4 marks get rid of im define it in where clause
flatten :: CNF -> CNFRep
flatten cnf
  = flatten' im cnf
  where
    im = idMap cnf
    flatten' im (Var i)
      = [[lookUp i im]]
    flatten' im (And f f')
      = (fim f) ++ (fim f')
      where
        fim = flatten' im
    flatten' im (Or f f')
      = [concat (fim f ++ fim f')]
      where
        fim = flatten' im
    flatten' im (Not (Var i))
      = [[negate (lookUp i im)]]
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits c
  | s == []   = (c, [])
  | otherwise =  (c'', s ++ i )
  where
    c' = remove c s
    s  = findSingleton c
    (c'', i) = propUnits c'

remove :: CNFRep -> [Int] -> CNFRep
remove [] s
  = []
remove (c : cs) s
  | elem r c    = next
  | elem (-r) c = [i | i <- c, i /= (-r)] : next
  | otherwise   = c : (remove cs s)
  where
    next = remove cs s
    r    = s!!0

-- Helper function for propUnits to find singleton clauses in a CNFRep
findSingleton :: CNFRep -> [Int]
findSingleton []
  = []
findSingleton (c1 : cs)
  | length c1 == 1 = c1
  | otherwise      = findSingleton cs

-- 4 marks
dp :: CNFRep -> [[Int]]
dp cnfr
  | elem [] cnfr1  && elem [] cnfr2 = []
  | elem [] cnfr1                   = [i ++ i2]
  | elem [] cnfr2                   = [i ++ i1]
  | otherwise                       = [i ++ i1, i ++ i2]
  where
    c = (head cnfr')!!0
    (cnfr', i)  = propUnits cnfr
    (cnfr1, i1) = dp' c cnfr'
    (cnfr2, i2) = dp' (-c) cnfr'


dp' :: Int -> CNFRep -> (CNFRep, [Int])

dp' c cnfr
  | cnfr' == []   = ([], i)
  | elem [] cnfr' = (cnfr', [])
  | otherwise     = (cnfr'', i ++ i')
  where
    (cnfr', i)   = propUnits ([c] : cnfr)
    (cnfr'', i') = dp' ((head cnfr')!!0) cnfr'



--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined
