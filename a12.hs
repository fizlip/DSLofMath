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
 deriving (Show)



eval :: (Show v, Eq v) => Env v Set -> SET v -> Set
eval env (Empty)              = S []
eval env (Var a)              = lookup' env a
eval env (Singleton s)        = S [eval env s]
eval env (Union s1 s2)        = unionSem (eval env s1) (eval env s2)
eval env (Intersection s1 s2) = intersectSem (eval env s1) (eval env s2)

-- Helper function for eval --
unionSem :: Set -> Set -> Set
unionSem (S s1) (S s2) = S (nub(s1++s2))
               
intersectSem :: Set -> Set -> Set 
intersectSem (S s1)(S s2) = S (nub(s1 `intersect` s2))

check :: (Show v,Eq v) => Env v Set -> PRED v -> Bool
check env (Elem s1 s2)    = elemSem (eval env s1) (eval env s2)
check env (Subset s1 s2)  = subsetSem2 (eval env s1) (eval env s2) 
check env (And p1 p2)     = (check env p1) && (check env p2)
check env (Or p1 p2)      = (check env p1) || (check env p2)
check env (Implies p1 p2) 
                          | (check env p1) && (check env p2)              = True
                          | (not (check env p1)) && (check env p2)        = True
                          | (not (check env p1)) && (not ( check env p2)) = True
                          | otherwise                                     = False
check env (Not p)         = not (check env p) 

elemSem :: Set -> Set -> Bool
elemSem (S [])  _     = True -- The empty set is a subset of every set.
elemSem _ (S [])      = False -- No elements in the empty set.
elemSem x (S [y] )    = x == y
elemSem x (S (y:ys))  = x == y || elemSem x (S ys)

subsetSem2 :: Set -> Set -> Bool
subsetSem2 (S [])     b  = elemSem (S []) b
subsetSem2 (S (x:xs)) b  = elemSem x b && subsetSem2 (S xs) b

--Testing--
ex1 = S [ S[S[S[]]] , S[S[]] , S[S[S[S[]]]] ]
e0 = S[]
e1 = S[ S[S[]] ]
e3 = S[S[]]
e4 = S[S[S[S[]]]]
e5 = S[S[S[]],S[S[]],S[ S[S[]],S[S[]]] ]

env = [(0, S []),(1,S[S[]]),(2,S[S[S[]]]), (3,S[S[S[S[]]]]),(4,S[S[S[S[S[]]]]])]

-- Task 3 von Neumann Encoding -- 
vN ::  Integer -> SET v
vN  0 = Empty
vN  n = Union (vN (n-1)) (Singleton (vN (n-1)))

vNSem ::(Show v, Eq v) =>  Env v Set -> Integer -> Set
vNSem env n = eval env (vN n)

-- If n1 <= n2 then n1 should be a subset of n2.
claim1 :: (Show v,Eq v) => Env v Set -> Integer -> Integer -> String
claim1 env n1 n2 = 
    if ( (n1 <= n2)  && check env (Subset (vN n1)(vN n2))) 
     then   show $ "If " ++ show n1 ++ " <= " ++ show n2 ++ " then " ++ show n1 ++ " is a subset of "     ++ show n2 
     else   show $ "If " ++ show n1 ++ " <= " ++ show n2 ++ " then " ++ show n1 ++ " is not a subset of " ++ show n2 


claim2 :: (Show v, Eq v) => Env v Set -> Integer -> String
claim2 env n = if(eval env (vN n) == S (foo env (claim2Helper n)))
               then show $ "The Von Neuman encoding of " ++ show n ++ " is equal to [0,1...," ++ show (n-1) ++ "]"
               else show $ "The Von Neuman encoding of " ++ show n ++ " is not equal to [0,1...," ++ show (n-1) ++ "]"


-- Takes an integer n and returns [vN  0, vN 1, vN 2,..., vN (n-1)], therefore 0 is undefined
-- For example n = 2 returns [vN 0, vN 1].    
claim2Helper :: Integer -> [SET v] 
claim2Helper 0 = error ("Undefined")
claim2Helper 1 = [Empty]
claim2Helper n = [vN (n-1)] ++ claim2Helper(n-1)

-- foo evalutes each element in the list that claim2Helper produces.
foo :: (Show v, Eq v) => Env v Set -> [SET v] -> [Set]
foo env a = map (eval env) a

-- Helper functions -- 
--Looks up "var" in "env" and returns "dom"
lookup' ::(Show v, Eq v) => Env v Set -> v -> Set
lookup' [ ] v = error ("lookup': variable " ++ show v ++ " not found")
lookup' ((x, y) : xys) v 
          | x == v    = y
          | otherwise = lookup' xys v

type Env var dom = [(var,dom)]

instance Eq Set where
 (==) = myEq
myEq :: Set -> Set      -> Bool
myEq   (S [])    (S []) = True
myEq   (S [])      _    = False
myEq   _         (S []) = False
myEq   (S [x])  (S [y]) = x == y
myEq a@(S xs) b@(S ys)  = subsetSem2 a b && subsetSem2 b a -- For example S [ S[], S[S[]]] == S [ S[S[]] , S[] ] will be true.
