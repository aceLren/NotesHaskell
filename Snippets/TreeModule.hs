module Tree where

data Tree = Branch Int Tree Tree | Node Int | Empty

total :: Tree -> Int
total (Branch v a b) = v + (total a) + (total b)
total (Node x) = x
total (Empty) = 0
