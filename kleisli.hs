

safe_root x = if x>=0 then Just (sqrt x)
             else Nothing

safe_reciprocal x = if x/=0 then Just (1/x)
                    else Nothing

m1 >=> m2 = \ x -> 
        let r1 = m1 x
            r2 = case r1 of Nothing -> Nothing
                            Just y -> m2 y
            in r2

compute = safe_root >=> safe_reciprocal