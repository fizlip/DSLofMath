import Data.List

data SET v =  Var           v 
            | Empty
            | Singleton    (SET v)
            | Union        (SET v) (SET v)
            | Intersection (SET v) (SET v)
            
 deriving Show
data PRED v = Elem     (SET v)  (SET v)
            | Subset   (SET v)  (SET v)
            | And      (PRED v) (PRED v) 
            | Or       (PRED v) (PRED v)
            | Implies  (PRED v) (PRED v)
            | Not      (PRED v)

newtype Set = S [Set]
 deriving (Eq,Show)

eval :: (Show v, Eq v) => Env v Set -> SET v -> Set
eval env (Empty) = S []
eval env (Var a) = lookup' env a
eval env (Singleton s) = S [eval env s]
eval env (Union s1 s2) = unionSem (eval env s1) (eval env s2)
eval env (Intersection s1 s2) = intersectSem (eval env s1) (eval env s2)

-- Helper function for eval --
unionSem :: Set -> Set -> Set
unionSem s1 s2 = S (nub((unpack s1)++(unpack s2)))
               
intersectSem :: Set -> Set -> Set 
intersectSem s1 s2 = S (nub((unpack s1) `intersect` (unpack s2)))

check :: (Show v,Eq v) => Env v Set -> PRED v -> Bool
check env (Elem s1 s2)    = elemSem (eval env s1) (eval env s2)
check env (Subset s1 s2)  = subsetSem env s1 s2
check env (And p1 p2)     = (check env p1) && (check env p2)
check env (Or p1 p2)      = (check env p1) || (check env p2)
check env (Implies p1 p2) 
                        | (check env p1) && (check env p2)              = True
                        | (not (check env p1)) && (check env p2)        = True
                        | (not (check env p1)) && (not ( check env p2)) = True
check env (Not p) = not (check env p) 

-- Helper functions for check --
elemSem :: Set -> Set -> Bool
elemSem s1 s2 = s1 `elem` (unpack s2) 

subsetSem ::(Show v, Eq v) =>  Env v Set -> SET v -> SET v -> Bool
subsetSem env s1 s2 = eval env s1 == eval env (Intersection s1 s2)


-- Task 3 von Neumann Encoding -- 
vN ::  Integer -> SET v
vN  0 = Empty
vN  n = Union (vN (n-1)) (Singleton (vN (n-1)))

vNSem ::(Show v, Eq v) =>  Env v Set -> Integer -> Set
vNSem env n = eval env (vN n)

claim1 :: (Show v,Eq v) => Env v Set -> Integer -> Integer -> Bool
claim1 env n1 n2 = (n1 < n2) &&  check env (Subset (vN n1)(vN n2))	

claim2 :: (Show v, Eq v) => Env v Set -> Integer -> Bool
claim2 _ 0 = True
claim2 env n = check env (Elem (vN (n-1)) (vN n)) && claim2 env (n-1)


-- Helper functions -- 
--Looks up "var" in "env" and returns "dom"
lookup' ::(Show v, Eq v) => Env v Set -> v -> Set
lookup' [ ] v = error ("lookup': variable " ++ show v ++ " not found")
lookup' ((x, y) : xys) v 
          | x == v    = y
          | otherwise = lookup' xys v

-- Calculates cardinality of a set
card :: [Set] -> Int
card [x]    = 1 
card (x:xs) = 1 + card xs 


--Unpacks a set. S [ S[] ] becomes S[]
unpack :: Set -> [Set]
unpack (S [])     = [S[]]
unpack (S [x])    = [x]
unpack (S (x:xs)) = x: (unpack (S xs))

type Env var dom = [(var,dom)]
