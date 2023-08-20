#!/usr/bin/env stack
-- stack --resolver lts-18.22 ghci --package QuickCheck --package containers-0.6.5.1

import Data.List (delete, sort, group)
import Data.Maybe (isJust, fromJust)

permutations :: (Ord a) => [a] -> [[a]]
permutations = permutations' . go
  where go :: (Ord a) => [a] -> [(a, Int)]
        go = map (\xs@(x:_) -> (x,length xs)) . group . sort

permutations' :: [(a, Int)] -> [[a]]
permutations' [] = [[]]
permutations' xs
    = [x:ys | (x,xs') <- select xs,
              ys      <- permutations' xs']
  where select [] = []
        select ((x,n):xs) = (x,xs') : [(y, (x,n):cs) | (y,cs) <- select xs]
          where xs' | 1 == n    = xs
                    | otherwise = (x, pred n) : xs

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs
  where subs = subsets xs

data BaseUnits = Dimensionless | TimeHours | MoneyPounds deriving (Show, Eq, Ord)
type DimExpr = (Expr Double, Expr BaseUnits)
data Expr a = Const a | Add (Expr a) (Expr a) | Sub (Expr a) (Expr a) | Mult (Expr a) (Expr a) | Div (Expr a) (Expr a) deriving (Show, Eq, Ord)

-- instance Num (Expr a) where
    -- x + y = x `Add` y
    -- x * y = x `Mult` y
    -- x - y = x `Sub` y
    -- abs = undefined
    -- fromInteger = undefined
    -- signum = undefined

-- instance Fractional (Expr a) where
    -- x / y = x `Div` y
    -- fromRational = undefined

allDimExprs :: [DimExpr] -> [DimExpr]
allDimExprs args = concat $ map go args'
  where args' = concat . map permutations . subsets $ args
        
        go :: [DimExpr] -> [DimExpr]
        go []    = []
        go [arg] = [arg]
        go args  = concat [[
                (Add l r, Add lUnits rUnits),
                (Sub l r, Sub lUnits rUnits),
                (Mult l r, Mult lUnits rUnits),
                (Div l r, Div lUnits rUnits)
            ] | i <- [1..(n-1)], (l,lUnits) <- go (take i args), (r,rUnits) <- go (drop i args)]
          where n = length args

eval :: Expr Double -> Double
eval (Const x) = x
eval (Add  e1 e2) = eval e1 + eval e2
eval (Sub  e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Div  e1 e2) = eval e1 / eval e2

showExpr :: (Show a) => Expr a -> String
showExpr (Const x)    = show x
showExpr (Add e1 e2)  = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Sub e1 e2)  = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Mult e1 e2) = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2)  = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"

showDimExpr (x,units) = show (eval x) ++ "\t" ++ showExpr x ++ "\t" ++ showExpr units

normaliseUnits :: Expr BaseUnits -> Maybe (Expr BaseUnits) -- puts units into standard form with at most one division occurring "last" (i.e. "most outside") and no additions: if two baseUnits are added which after normalisation are unequal then return Nothing.
normaliseUnits = fmap fromNumersDenoms . toNumersDenoms
  where toNumersDenoms :: Expr BaseUnits -> Maybe ([BaseUnits], [BaseUnits])
        toNumersDenoms (Const x) = return $ ([x],[])
        toNumersDenoms (Add  e1 e2)
            = do e1' <- toNumersDenoms e1
                 e2' <- toNumersDenoms e2
                 if e1' == e2'
                  then return e1'
                  else Nothing
        toNumersDenoms (Sub  e1 e2) = toNumersDenoms (Add  e1 e2)
        toNumersDenoms (Mult e1 e2)
            = do (numers1,denoms1) <- toNumersDenoms e1
                 (numers2,denoms2) <- toNumersDenoms e2
                 
                 let numers = filter (/= Dimensionless) $ numers1 ++ numers2
                 let denoms = filter (/= Dimensionless) $ denoms1 ++ denoms2
                 
                 let numers' = foldr delete numers denoms
                 let denoms' = foldr delete denoms numers
                 
                 return (numers', denoms')
        toNumersDenoms (Div  e1 e2)
            = do (numers1,denoms1) <- toNumersDenoms e1
                 (numers2,denoms2) <- toNumersDenoms e2
                 
                 let numers = filter (/= Dimensionless) $ numers1 ++ denoms2
                 let denoms = filter (/= Dimensionless) $ denoms1 ++ numers2
                 
                 let numers' = foldr delete numers denoms
                 let denoms' = foldr delete denoms numers
                 
                 return (numers', denoms')
        
        fromNumersDenoms :: ([BaseUnits], [BaseUnits]) -> Expr BaseUnits
        fromNumersDenoms (numers, denoms)
            | denom == Const Dimensionless = numer
            | otherwise                    = Div numer denom
                 
          where numer = multiply numers
                denom = multiply denoms
                
                multiply []     = Const Dimensionless
                multiply [x]    = Const x
                multiply (x:xs) = Mult (Const x) (multiply xs)

printMatchingDimExprs target args = printDimExprs . removeSndFailures . normaliseDimExprs . keepTarget $ allDimExprs args
  where keepTarget = filter ((== target) . eval . fst)

printNearDimExprs target tol args = printDimExprs . removeSndFailures . normaliseDimExprs . keepNear $ allDimExprs args
  where keepNear = filter ((< tol) . abs . (subtract target) . eval . fst)

normaliseDimExprs = map (fmap normaliseUnits)
removeSndFailures = map (fmap fromJust) . filter (isJust . snd)
printDimExprs = mapM_ (putStrLn . showDimExpr)

printAllDimExprs args = printDimExprs . removeSndFailures . normaliseDimExprs $ allDimExprs args

exampleProblemArgs = [
        (Const 1, Const Dimensionless),
        (Const 1, Const Dimensionless),
        (Const 1, Const TimeHours),
        (Const 1, Const TimeHours)
    ]

printAllDimExprsFromExampleProblemArgs = printAllDimExprs exampleProblemArgs

exampleProblem = printMatchingDimExprs target exampleProblemArgs
  where target = 3

exampleProblemApprox = printNearDimExprs target tol exampleProblemArgs
  where target = 12.5
        tol = 0.05
