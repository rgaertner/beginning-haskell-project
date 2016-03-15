module Chapter3.Functions where

(***) :: (a -> c) -> (b -> d) -> ((a,b) -> (c,d))
f *** g = \(x,y) -> (f x,g y)

duplicate :: a -> (a,a)
duplicate x = (x,x)

formular :: Integer -> Integer
formular = uncurry (+) . ((*3) *** ((*7) . (+2))) . duplicate

filterung :: (a -> Bool) -> [a] -> [a]
filterung f [] = [] 
filterung f (x:xs) 
    | f x = x : (filterung f xs)
    | otherwise = filterung f xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs 
