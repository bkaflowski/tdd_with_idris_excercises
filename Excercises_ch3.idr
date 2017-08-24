import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         zipWith (\el, vect => el :: vect) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (\a, b => a + b) x y :: addMatrix xs ys

computeRow : Num a => Vect m a -> Vect k (Vect m a) -> Vect k a
computeRow xs [] = []
computeRow xs (y :: ys) = sum (zipWith (*) xs y) :: computeRow xs ys

multMatrix: Num a => Vect n (Vect m a) -> Vect m (Vect k a) -> Vect n (Vect k a)
multMatrix [] ys = []
multMatrix (x :: xs) ys = let rightTrans = transposeMat ys in
                          computeRow x rightTrans :: multMatrix xs ys
