main = print (tarai 20 10 5)

tarai :: Int -> Int -> Int -> Int
tarai x y z = if x <= y then y
                        else tarai (tarai (x-1) y z)
                                   (tarai (y-1) z x)
                                   (tarai (z-1) x y)
