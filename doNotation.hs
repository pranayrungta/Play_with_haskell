-- do is not as good as imperative style code
-- we may feel that second function will succeed 
-- but it fails

succeed a = do
    x <- Just 3
    y <- Just 4
    Just (x+a)


failSure a = do
    x <- Just 89
    y <- Nothing
    z <- Just 21
    Just (x+a)
