counts: String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

over_length : Nat -> List String -> Nat
over_length n l = length (filter (> 3) (map length l))
