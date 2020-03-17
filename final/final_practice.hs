module Final where
import Prelude hiding (map)



f :: Int -> Int -> Int
f x y = x + y

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs


