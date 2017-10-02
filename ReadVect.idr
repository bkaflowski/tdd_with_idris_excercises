import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

testReadVect : IO ()
testReadVect = do output <- readVectLen 4
                  putStrLn(show output)

data VectUnknown : Type -> Type where
     MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if (x == "")
                 then pure (MkVect _ [])
                 else do MkVect _ xs <- readVect
                         pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs)
      = putStrLn (show xs ++ " (length " ++ show len ++ ")")

mypair : (Int, String)
mypair = (94, "Pages")

anyVect : (n ** Vect n String)
anyVect = (3 ** ?anyVect_rhs)

readVect2 : IO (len ** Vect len String)
readVect2 = do x <- getLine
               if (x == "")
                  then pure (_ ** [])
                  else do (_ ** xs) <- readVect2
                          pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- readVect2
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- readVect2
               case exactLength len1 vec2 of
                  Nothing => putStrLn "Vectors are of different lengths"
                  Just vec2' => printLn (zip vec1 vec2')
