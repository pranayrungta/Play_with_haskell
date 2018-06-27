data Exception b = Log String | Value b deriving(Show)

instance Functor Exception where
    fmap = liftM
 
instance Applicative Exception where
    pure  = Value
    (<*>) = ap
 
instance Monad Exception where
   returns = pure -- redundant since GHC 7.10 due to default impl
   (Log  x) >>= f = Log x
   (Value x) >>= f = f x


safe_root x = if x>=0 then Value (sqrt x)
               else Log "failed at sq root"

safe_reciprocal x = if x/=0 then Value (1/x)
                     else Log "failed at reciprocal"

compute x = return x >>= safe_root >>= safe_reciprocal
