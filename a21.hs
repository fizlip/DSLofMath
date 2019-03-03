newtype Tri a    = Tri (a, a ,a)
 deriving Show

type TriFun a    = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a    = a -> Tri a   -- = a -> (a,a,a)

type R = Double
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


addTri :: Num a => Tri a -> Tri a -> Tri a
addTri (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f+g) , (f' + g') , (f'' + g''))


mulTri :: Num a => Tri a -> Tri a -> Tri a
mulTri  (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f * g) , (f' * g + f * g') , g*f'' + f'*g'*f'*g' + f*g'')

minTri :: Tri a -> Tri a -> Tri a
minTri = undefined

fromIntegerTri :: Integer -> Tri a
fromIntegerTri = undefined

sinTri :: (Num a, Floating a) =>  Tri a -> Tri a
sinTri (Tri (f, f', f'')) = Tri (sin f, cos f', -sin f'')

cosTri :: (Num a, Floating a) => Tri a -> Tri a
cosTri (Tri (f,f', f'')) = Tri ( -sin f, -cos f', sin f'')

expTri :: Tri a -> Tri a 
expTri (Tri (f, f', f'')) = undefined

fromRationalTri :: Rational -> Tri a
fromRationalTri = undefined

divTri ::(Num a, Fractional a) =>  Tri a -> Tri a -> Tri a
divTri (Tri (f, f', f'')) (Tri (g, g', g'')) = Tri( (f / g) ,  (((g * f' ) - (f * g')) / g*g)       ,     ((g*g*f'' - g*f'*f'*g' - f*g''*g + f*f*g'*g') / g*g*g ))

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

type Func = R -> R

derive :: FunExp -> FunExp 
derive (Const a)   = Const 0
derive Id          = Const 1
derive (e1 :*: e2 :+: e3 :*: e4) = (derive (e1 :*: e2)) :+: (derive (e3 :*: e4)) -- a*b + c*d 
derive (e1 :+: e2) = (derive e1 :+: derive e2)
derive (e1 :*: e2) = (derive e1 :*: e2) :+: (e1 :*: derive e2)
derive (Exp e)     = derive e :*: Exp e
derive (Sin e)     = derive e :*: Cos (e)
derive (Cos e)     = derive e :*: (Const (-1) :*: Sin (e))

eval :: FunExp -> Func
eval (Const a)   = const a
eval Id          = id
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (Exp e)     = exp (eval e)
eval (Sin e)     = sin (eval e)
eval (Cos e)     = cos (eval e)

eval' :: FunExp -> Func 
eval' = eval.derive

eval'' :: FunExp -> Func
eval'' = eval.derive.derive

evalDD :: FunExp -> Tri Func
evalDD e = Tri (eval e, eval' e, eval'' e)


--Strings. Chap 4. s.s73 --
data E = Add E E 
       | Mul E E 
       | Con Integer 
 deriving Eq
 
prTop :: E -> String 
prTop e = let (pTop,_,_) = prVersions e
          in pTop

type TV = (String, String, String) -- Three Versions

prVersions :: E -> TV
prVersions = foldE prVerAdd prVerMul prVerCon

prVerAdd :: TV -> TV -> TV
prVerAdd (xTop, xInA,   xInM) (yTop, yInA, yInM) =
  let s = xInA ++ "+" ++ yInA -- use InA because we are "in Add"
  in (s, paren s, paren s)

prVerMul :: TV -> TV -> TV 
prVerMul (xTop, xInA, xInM) (yTop, yInA, yInM) =
  let s = xInM ++ "*" ++ yInM -- use InM because we are in "in Mul"
  in (s, s, paren s)          -- parents only needed inside Mul

prVerCon :: Integer -> TV
prVerCon i = 
  let s = show i
  in (s,s,s)

paren :: String -> String 
paren s = "(" ++ s ++ ")"


foldE :: (s -> s -> s) -> (s -> s -> s) -> (Integer -> s ) -> (E -> s)
foldE add mul con = rec
 where rec (Add x y) = add (rec x) (rec y)
       rec (Mul x y) = mul (rec x) (rec y)
       rec (Con i)   = con i
 

evalE1 :: E -> Integer
evalE1 = foldE (+) (*) id

evalE2 :: Num a => E -> a
evalE2 = foldE (+) (*) fromInteger

idE :: E -> E 
idE = foldE Add Mul Con


class IntExp t where
 add :: t -> t -> t
 mul :: t -> t -> t
 con :: Integer -> t

foldIE :: IntExp t => E -> t
foldIE = foldE add mul con 

instance IntExp E where
  add = Add
  mul = Mul
  con = Con

instance IntExp Integer where
 add = (+)
 mul = (*)
 con = id

idE' :: E -> E
idE' = foldIE

evalE' :: E -> Integer
evalE' = foldIE

-- Examples --
seven :: IntExp a => a
seven = add (con 3) (con 4)

testI :: Integer 
testI = seven

testE :: E 
testE = seven

--End of Examples

-- Chap 4. s.78 --

instance Num FunExp where 
  (+)           = (:+:)
  (*)           = (:*:)
  fromInteger   = Const . fromInteger
  abs           = error "not implemented"
  signum        = error "not implemented"
  negate        = error "not implemeneted"

instance Fractional FunExp where 
    recip e      = Const 1 :/: e    -- returns 1 / argument
    fromRational = Const . fromRational

instance Floating FunExp where 
  exp   = Exp
  sin   = Sin
  cos   = Cos
  pi    = pi
  log   = log 
  asin  = asin
  acos  = acos
  atan  = atan
  sinh  = sinh 
  cosh  = cosh 
  asinh = asinh 
  acosh = acosh 
  atanh = atanh


class GoodClass t where
  constF :: R -> t
  addF   :: t -> t -> t
  mulF   :: t -> t -> t
  divF   :: t -> t -> t
  expF   :: t -> t
  idF    :: t
  sinF   :: t -> t
  cosF   :: t -> t

instance Num a => GoodClass (Tri (a)) where
 constF = evalDDConst
 addF   = evalDDApp
 mulF   = evalDDMul
 divF   = evalDDDiv
 expF   = evalDDExp
 idF    = evalDDId
 sinF   = evalDDSin
 cosF   = evalDDCos


evalDDConst = error "not implemented"
evalDDApp   = error "not implemented"
evalDDMul   = error "not implemented"
evalDDDiv   = error "not implemented"
evalDDExp   = error "not implemented"
evalDDId    = error "not implemented"
evalDDSin   = error "not implemented"
evalDDCos   = error "not implemented"

instance GoodClass FunExp where
  constF = Const
  idF  = Id
  addF = (:+:)
  mulF = (:*:)
  divF = (:/:)
  expF = Exp
  sinF = Sin
  cosF = Cos


{- Kompilerar inte s.80 
instance GoodClass (R -> R) where
  constF = const
  idF  = id
  addF = (+)
  mulF = (*)
  ...
  -}

{- In Sec. 3.6.1, we dened a Num instance for functions with a Num codomain. If we have an
element of the domain of such a function, we can use it to obtain a homomorphism from functions
to their codomains: -}
 
 --Num a => x -> (x -> a) -> a

 {- As suggested by the type, the homomorphism is just funciton application: -}

{- Kompilerar inte -

apply :: a -> (a -> b) -> b
apply a = \f -> f a

om h = apply for some fixed c we have, 

 h ( f + g) = {- def apply -}
 (f + g ) c = {- def + for functions -}
 f c + g c  = {- def apply -}
 h f + h g etc.

 -}

applyTri :: a -> Tri a -> (a,a,a)
applyTri c      (Tri (f, f', f'')) = Tri (f c, f' c, f'' c)


 
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

