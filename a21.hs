newtype Tri a    = Tri (a,a,a)
 deriving Show
type TriFun a    = Tri (a -> a) -- = (a -> a, a -> a, a -> a)
type FunTri a    = a -> Tri a   -- = a -> (a,a,a)


-- (Funktions värde, värde på f', värde på f'')
data FunExp = Const Rational
             | Id
             | FunExp :+: FunExp
             | FunExp :*: FunExp
             | FunExp :/: FunExp
             | Exp FunExp
             | Sin FunExp
             | Cos FunExp
            deriving (Eq,Show)



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

mulTri :: Num a => Tri a -> Tri a -> Tri a
mulTri  (Tri (f,f',f'')) (Tri (g,g',g'')) = Tri ( (f * g) , (f' * g + f * g') ,  (f'' * g + f' * g' ) + (f' * g' + f * g'')  )

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
divTri (Tri (f, f', f'')) (Tri (g, g', g'')) = Tri( (f / g) , ((g * f' ) - (f * g')) / g*g , undefined  )

evalTri :: Tri a -> Integer
evalTri = undefined

evalDD :: FunExp -> FunTri a
evalDD = undefined

