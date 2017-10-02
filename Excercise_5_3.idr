import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure ([])
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do cnt <- readToBlank
                 putStr "File name: "
                 fn <- getLine
                 Right r <- writeFile fn (unlines cnt) | Left err => putStrLn (show err)
                 pure r


readVectStr : (f : File) -> IO (len ** Vect len String)
readVectStr f = do Right x <- fGetLine f | Left err => pure (_ ** [])
                   isEof <- fEOF f
                   putStrLn (show x)
                   if (isEof == True || x == "")
                   then pure (_ ** [])
                   else do (_ ** xs) <- readVectStr f
                           pure (_ ** x :: xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile fn = do Right f <- openFile fn Read | Left err => pure (_ ** [])
                     res <- readVectStr f
                     closeFile f
                     pure res
