data Exception b = Log String | Value b deriving(Show)

instance Functor Exception where
    fmap f (Log msg)   = Log msg
    fmap f (Value b)   = Value (f b)
 
instance Applicative Exception where
    pure  = Value
    (Log msg) <*> nextValue = Log msg
    (Value f) <*> nextValue = fmap f nextValue
 
instance Monad Exception where
   return = pure
   (Log  x) >>= f = Log x
   (Value x) >>= f = f x


safeRoot' x = if x>=0 then Value (sqrt x)
              else Log "failed at sq root"

safeReciprocal' x = if x/=0 then Value (1/x)
                     else Log "failed at reciprocal"

compute x = return x >>= safeRoot' >>= safeReciprocal'
-- TRY THESE EXAMPLES at ghci: 
--  compute 23
--  compute (-3)
--  compute 0
