import Data.Vect

search : String -> Vect m String -> List String
search pat [] = []
search pat (x :: xs) = if isInfixOf pat x then x :: search pat xs
                       else search pat xs
