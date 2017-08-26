module Main

import BSTree

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ val :: (treeToList right)

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (evaluate x) + (evaluate y)
evaluate (Sub x y) = (evaluate x) - (evaluate y)
evaluate (Mult x y) = (evaluate x) * (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case compare x y of
                             LT => Just y
                             EQ => Just x
                             GT => Just x
