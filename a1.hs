data SET v = Empty
            | Singleton v
            | Union (SET v) (SET v)
            | Intersection (SET v) (SET v)
            | Var v

data PRED v = Elem (SET v) (SET v)
            | Subset (SET v) (SET v)
            | And (PRED v) (PRED v) 
            | Or (PRED v) (PRED v)
            | Implies (PRED v) (PRED v)
            | Not (PRED v)

newtype Set = S [Set]
    deriving(Eq, Show)

-- Get a given set as a list of sets
getList :: Set -> [Set]
getList (S x) = x

isPresent :: Set -> [Set] -> Bool
isPresent a (x:xs)
    | a == x = True
    | otherwise = isPresent a xs
isPresent a [] = False

-- Evaluator for a set expression. 
eval :: Eq v => Env v Set -> SET v -> Set
eval env (Empty)                = S []
eval env (Singleton a)          = S ((S []):[])
eval env (Union a1 a2)          = S ((eval env a1):(eval env a2):[])
eval env (Intersection a1 a2)   = S ([x | x <- getList(eval env a1), y <- getList(eval env a2), x == y])
eval env (Var a) = find env a 
    where   find ((x,y):xss) b 
                | x == b        = y
                | otherwise     = find xss b
            find [] b           = S []  -- If variable is not found return an empty set

check :: Eq v => Env v Set -> PRED v -> Bool
check env (Elem var set)    = isPresent (eval env var) (getList (eval env set))
check env (Subset s1 s2)    
                        | (eval env s1) == (eval env (Intersection s1 s2)) = True
                        | otherwise = False
check env (And p1 p2)       = (check env p1) && (check env p2)
check env (Or p1 p2)        = (check env p1) || (check env p2)
check env (Not p1)          = not (check env p1)
check env (Implies p1 p2) 
                        | (check env p1) && (check env p2) = True
                        | (not (check env p1)) && (check env p2) = True
                        | otherwise = False


type Env var dom = [(var, dom)]
