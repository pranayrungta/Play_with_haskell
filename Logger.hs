data Logger a = Logger String a deriving(Show)

p = 9
q = Logger "Nine" 9

square t = t*t
sumIt p q = p + q

instance Functor Logger where
    fmap f (Logger msg a) = Logger msg (f a)

squareLog x = fmap square x
-- TRY EXAMPLE :
-- squareLog q

instance Applicative Logger where
    pure  = Logger ""
    (Logger msg1 f) <*> (Logger msg2 a) = Logger (msg1++msg2) (f a)

-- EXAMPLE OF Applicative FUNCTOR
r = pure (*) <*> Logger "mult three " 3 <*> Logger "and four" 4


instance Monad Logger where
    return = pure
    (Logger msg1 a) >>= f = let Logger msg2 b = f a
                            in Logger (msg1++msg2) b

square' t = Logger "squaring " (t*t)
cube' t = Logger "cubing" (t*t*t)

compose' x = return x >>= square' >>= cube'
-- TRY compose' 4