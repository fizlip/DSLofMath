import Data.List

newtype Tri a    = Tri (a, a ,a)
 deriving (Show, Eq)



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
  abs         = error "not implemented"
  signum      = error "not implemented"


instance Fractional a => Fractional (Tri a) where 
 (/)          = divTri
 fromRational = fromRationalTri



instance Floating a => Floating (Tri a) where
 pi    = error "not implemented"
 log   = error "not implemented"
 exp   = expTri
 sin   = sinTri
 cos   = cosTri
 asin  = error "not implemented"
 acos  = error "not implemented"
 atan  = error "not implemented"
 sinh  = error "not implemented"
 cosh  = error "not implemented"
 asinh = error "not implemented"
 acosh = error "not implemented"
 atanh = error "not implemented"


addTri :: Num a => Tri a -> Tri a -> Tri a
addTri (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f+g) , (f' + g') , (f'' + g''))


mulTri :: Num a => Tri a -> Tri a -> Tri a
mulTri  (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f * g) , (f' * g + f * g') , g*f'' + f'*g'*f'*g' + f*g'')

minTri :: Num a =>  Tri a -> Tri a -> Tri a
minTri (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f-g) , (f' - g') , (f'' - g''))

fromIntegerTri :: Num a =>  Integer -> Tri a
fromIntegerTri i = Tri (fromInteger i, 0,0)

sinTri :: (Num a, Floating a) =>  Tri a -> Tri a
sinTri (Tri (f, f', f'')) = Tri (sin f, f' * cos f, (f'' * cos f) + f' *(-sin f))

cosTri :: (Num a, Floating a) => Tri a -> Tri a
cosTri (Tri (f,f', f'')) = Tri (cos f, f' * (-sin f), (f'' * (-sin f)) + (f' *(-cos f)))

expTri :: (Num a, Floating a) =>Tri a -> Tri a 
expTri (Tri (f, f', f'')) = Tri (exp f, exp f * f', exp f * f' + exp f * f'')

fromRationalTri :: (Fractional a, Num a) => Rational -> Tri a
fromRationalTri r = Tri (fromRational r, 0,0)

divTri ::(Num a, Fractional a) =>  Tri a -> Tri a -> Tri a
divTri (Tri (f, f', f'')) (Tri (g, g', g'')) = Tri( (f / g) ,  (((g * f' ) - (f * g')) / g*g)       ,     ((g*g*f'' - g*f'*f'*g' - f*g''*g + f*f*g'*g') / g*g*g ))


--type Func = R -> R

-- a -> (a,a,a) <=> (a -> a, a -> a, a -> a)

evalDD :: (Floating a) => FunExp -> FunTri a
evalDD (Const a)   = \x -> Tri (x,0,0)
evalDD Id          = \x -> Tri (x,1,0)
evalDD (e1 :+: e2) =  evalDD e1 + evalDD e2
evalDD (e1 :*: e2) =  evalDD e1 * evalDD e2
evalDD (e1 :/: e2) =  evalDD e1 / evalDD e2
evalDD (Sin e)     =  sin (evalDD e)
evalDD (Cos e)     =  cos (evalDD e)
evalDD (Exp e)     =  exp (evalDD e)


--evalDD Sin (Id :*: Id)
a :: FunTri Double
a = evalDD (f1 :*: f2)

b :: FunTri Double
b = evalDD f1 * evalDD f2

check :: Double -> Bool
check x = applyTri a x == applyTri b x

checkerino :: [Double]
checkerino = [ x | x <- [-1000..1000], not $ check x]
--evalute LH: evalDD ( x :*: y ) -> 

applyTri :: Floating a =>  FunTri a -> a ->  Tri a
applyTri f c = f c

--Testing--
f1 = Id :*: Id
f2 = Sin Id

e1 = evalDD (Id :*: Id)
e2 = evalDD (Sin Id)
e3 = evalDD (Id)

instance Num a => Num ( x -> a) where
  f + g = \x -> f x + g x
  f - g = \x -> f x - g x
  f * g = \x -> f x * g x
  negate f    = negate.f
  abs f       = abs.f
  signum f    = signum.f
  fromInteger = const.fromInteger

instance Fractional a => Fractional (x -> a) where
  recip f      = recip f
  fromRational = const . fromRational

instance Floating a => Floating (x -> a) where
  pi      = const pi
  exp f   = exp . f
  f**g    = \x -> (f x) ** (g x)
  sin f   = sin . f
  cos f   = sin . f
  log f   = log . f
  asin f  = asin . f
  acos f  = acos . f
  atan f  = atan . f
  sinh f  = sinh . f
  cosh  f = cosh . f
  asinh f = asinh . f
  acosh f = acosh . f
  atanh f = atanh . f


apply :: (Tri R -> FunExp -> Tri R)
apply = undefined

newtonTri :: (Tri R -> Tri R) -> R -> R -> R
newtonTri f c x = if abs fx < c
                    then x
                    else if fx' /= 0 then newtonTri f c next
                                        else newtonTri f c ( x + c )
          where  fx   = sndTri (f (Tri (x,1,0)))
                 fx'  = thdTri (f (Tri (x,1,0)))
                 next = x - (fx / fx')




fstTri :: Tri a -> a
fstTri (Tri (x,_,_)) = x

sndTri :: Tri a -> a
sndTri (Tri (_,x,_)) = x

thdTri :: Tri a -> a
thdTri (Tri (_,_,x)) = x
{-
test0 :: Tri R -> Tri R
test0 (Tri (x,1,0)) = Tri (x*x, 2*x, 2)

test1 :: Tri R -> Tri R
test1 (Tri (x,1,0)) = Tri (x^2-1, 2*x, 2)

test2 :: Tri R -> Tri R
test2 (Tri (x,1,0)) = Tri (sin x, cos x, -sin x)
-}

test0 x = x^2

test1 x = x^2 -1

test2 x = sin x

newtonT f = map (newtonTri f 0.001) [-2.0, -1.5..2.0]

optimT f = map (optim f 0.01) [-2.0, -1.5..4.0]

newtonList :: (Fractional a, Ord a) =>  (a -> a) -> a -> a -> [a]
newtonList f c x = undefined -- map (newtonTri f c) [-2.0, -1.5..2.0]

--newton f c x = last (newtonList f c x)

--newton = newtonTri f x c >= -c && newtonTri f x c <= c 

data Result a = Maximum R | Minimum R | Dunno R
 deriving Show

optim :: (Tri R -> Tri R) -> R -> R -> Result R
optim f e x   | sndDeriv < 0 = Maximum y
              | sndDeriv > 0 = Minimum y
              | otherwise   = Dunno y

   where xTurn = newtonTri f e x
         fx    = f (Tri (xTurn, 1,0))
         y     = fstTri (fx)
         sndDeriv = thdTri (fx)       
                  -- if ( newtonTri f' x c < 0) then Minimum (newtonTri f' x c)
{-
 
 optim -> a use newtonTri to find zer of f' starting from x. 
           if( y is within c of 0) then a = y

           if ( f'' y < 0 ) then min y
           if (f'' y  > 0 ) then max y
           else Dunno y 
-}
