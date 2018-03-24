import Data.List

minfree xs = minform 0 (length xs, xs)

minform a (n, xs) | n==0 = a
                  | m == b-a = minform b (n-m, vs)
                  | otherwise = minform a (m, us)
                  where (us,vs) = partition (<b) xs
                        b = a+1+n `div` 2
                        m=length us
                        
main = putStrLn $ show (minfree([0,2,3,4]))
