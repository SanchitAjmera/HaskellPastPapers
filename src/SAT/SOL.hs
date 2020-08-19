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

-- 4 marks
flatten :: CNF -> CNFRep
flatten cnf
  = flatten' im cnf
  where
    im = idMap cnf
    flatten' :: IdMap -> CNF -> CNFRep
    flatten' im (Var i)
      = [[lookUp i im]]
    flatten' im (And f f')
      = (fim f) ++ (fim f')
      where
        fim = flatten' im
    flatten' im (Or f f')
      = [(fim f)!!0 ++ (fim f')!!0]
      where
        fim = flatten' im
    flatten' im (Not (Var i))
      = [[-(lookUp i im)]]
--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits
  = undefined

-- 4 marks
dp :: CNFRep -> [[Int]]
dp
  = undefined

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
allSat
  = undefined
