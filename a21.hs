newtype Tri a    = Tri (a, a ,a)
 deriving Show
type TriFun a    = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a    = a -> Tri a   -- = a -> (a,a,a)


-- (Funktions värde, värde på f', värde på f'')
data FunExp = Const Double
             | Id
             | FunExp :+: FunExp
             | FunExp :*: FunExp
             | FunExp :/: FunExp
             | Exp FunExp
             | Sin FunExp
             | Cos FunExp 
            deriving (Eq)
          
instance Show FunExp where -- show instance for FunExp, does not handle paranatheses.
  show Id                 = "x"
--  show (Const 0 :*: e)    = ""
  show (Const 0 :+: Const 1) = "1"
  show (Const 1 :*: e)    = show e
  show (e :*: Const 1)    = show e
  show (Const (-1) :*: e) = "-" ++ show e
  show (Const (-1))       = "-"


  show (Const a)   = show a
  show (e1 :+: e2) = show e1 ++ "+" ++ show e2
  show (e1 :*: e2) = show e1 ++ "*" ++ show e2
  show (Exp e)     = "e^(" ++ show e ++ ")"
  show (Sin e)     = "sin (" ++ show e ++ ")"
  show (Cos e)     = "cos(" ++ show e ++ ")"

-- Define :: Num, Fractional, Floating and homorphism for evalDD

instance Num a => Num (Tri a) where
  (+)         = addTri
  (*)         = mulTri
  (-)         = minTri
  fromInteger = fromIntegerTri
  abs         = error "no abs pls"
  signum      = error "no signum either pls"


instance Fractional a => Fractional (Tri a) where 
 (/)          = divTri
 fromRational = fromRationalTri



instance Floating a => Floating (Tri a) where
 pi    = error "pi pls"
 log   = error "no blyat"
 exp   = expTri
 sin   = sinTri
 cos   = cosTri
 asin  = error "undefined"
 acos  = error "undefined"
 atan  = error "undefined"
 sinh  = error "undefined"
 cosh  = error "undefined"
 asinh = error "undefined"
 acosh = error "undefined"
 atanh = error "undefined"


  -- (f, f', f'') :+: (g,g',g'') = (h,h',h'')
-- h  = f + g 
-- h' = f' + g'
-- h'' = f'' + g''



addTri :: Num a => Tri a -> Tri a -> Tri a
addTri (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f+g) , (f' + g') , (f'' + g''))

-- (f, f', f'') :*: (g,g',g'') = (h,h',h'')
-- h = f * g 
-- h' = D(f * g) = f' * g + f * g'
-- h'' = DD ( f * g ) = D (f' * g + f * g') = (f'' * g + f' * g') + (f' * g' + f * g'') 
-- (f'' * g + f' * g' ) + (f' * g' + f * g'')
mulTri :: Num a => Tri a -> Tri a -> Tri a
mulTri  (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f * g) , (f' * g + f * g') , g*f'' + f'*g'*f'*g' + f*g'')

minTri :: Tri a -> Tri a -> Tri a
minTri = undefined

fromIntegerTri :: Integer -> Tri a
fromIntegerTri = undefined

-- (f, f', f'') => (h, h', h'')
--h = sin f
--h' = cos f'
--h'' = -sin f''

sinTri :: (Num a, Floating a) =>  Tri a -> Tri a
sinTri (Tri (f, f', f'')) = Tri (sin f, cos f', -sin f'')

cosTri :: (Num a, Floating a) => Tri a -> Tri a
cosTri (Tri (f,f', f'')) = Tri ( -sin f, -cos f', sin f'')

expTri :: Tri a -> Tri a 
expTri (Tri (f, f', f'')) = undefined

fromRationalTri :: Rational -> Tri a
fromRationalTri = undefined

-- (f, f', f'') / (g, g', g'') => (h, h', h'')
-- h = f / g
-- h' = D ( f / g ) = ((g * f' ) - (f * g')) / g*g
-- h'' = DD ( f / g) = D ( ((g * f' ) - (f * g')) / g*g ) = 


divTri ::(Num a, Fractional a) =>  Tri a -> Tri a -> Tri a
divTri (Tri (f, f', f'')) (Tri (g, g', g'')) = Tri( (f / g) ,  (((g * f' ) - (f * g')) / g*g)       ,     ((g*g*f'' - g*f'*f'*g' - f*g''*g + f*f*g'*g') / g*g*g ))


--type FunTri a    = a -> Tri a   -- = a -> (a,a,a)
evalDD :: FunExp -> FunTri a -- FunExp -> (FunExp -> (FunExp ,FunExp , FunExp,))
evalDD e  = undefined--Tri(eval e, eval (derive e), eval (derive (derive e)))


deriveTripple :: FunExp -> (FunExp, FunExp, FunExp)
deriveTripple e = (e, derive e, derive (derive e))

{-

newtype Tri a    = Tri (a, a ,a)
 deriving Show
type TriFun a    = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a    = a -> Tri a   -- = a -> (a,a,a)

-}

derive :: FunExp -> FunExp 
derive (Const a)   = Const 0
derive Id          = Const 1
derive (e1 :*: e2 :+: e3 :*: e4) = (derive (e1 :*: e2)) :+: (derive (e3 :*: e4)) -- a*b + c*d 
derive (e1 :+: e2) = (derive e1 :+: derive e2)
derive (e1 :*: e2) = (derive e1 :*: e2) :+: (e1 :*: derive e2)
derive (Exp e)     = derive e :*: Exp e
derive (Sin e)     = derive e :*: Cos (e)
derive (Cos e)     = derive e :*: (Const (-1) :*: Sin (e))
  



--s.65 chap 3--

instance Num a => Num ( x -> a) where
  f + g = \x -> f x + g x
  f - g = \x -> f x - g x
  f * g = \x -> f x * g x
  negate f = negate.f
  abs f    = abs.f
  signum f = signum.f
  fromInteger = const.fromInteger

instance Fractional a => Fractional (x -> a) where
  recip f = recip f
  fromRational = const . fromRational

instance Floating a => Floating (x -> a) where
  pi = const pi
  exp f = exp.f
  f**g  = \x -> (f x) ** (g x)
  sin   = \x -> sin x
  cos   = \x -> cos x
  log   = \x -> log x
  asin  = \x -> asin x
  acos  = \x -> acos x
  atan  = \x -> atan x
  sinh  = \x -> sinh x
  cosh  = \x -> cosh x
  asinh = \x -> asinh x
  acosh = \x -> acosh x
  atanh = \x -> atanh x
{-
type Func = Double -> Double

eval ::  FunExp -> Double
eval (Const a)   = a
eval Id          = id
eval (e1 :+: e2) = (eval e1) + (eval e2)
eval (e1 :*: e2) = eval e1 * eval e2
eval (Exp e)     = exp (eval e)
-}
--Testing--

e1 = Id :*: Id -- x^2 -- deriveTripple returns correct answer.

e2 = Sin Id -- sin x -- deriveTripple returns correct

e3 = Sin Id :*: Cos Id --sin x * cos x -- deriveTripple returns correct

e4 = Id :*: Id :+: Const 2 :*: Id -- x * x + 2 * x -- derive returns correct as long as parantheses are correct.
e5 = (Id :*: Id) :+: (Const 2 :*: Id) -- derive e4 == derive e5

e6 = Exp (Id :*: Id) :+: (Const 2 :*: Id) -- e^(x^2 + 2x)
-- evalDD (x * y) = evalDD :*: evalDD y
-- * :: FunExp -> FunExp
-- :*:  :: Tri a -> Tri a

