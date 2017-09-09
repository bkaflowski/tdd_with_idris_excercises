printLonger : IO ()
printLonger = do putStr "First string: "
                 first <- getLine
                 putStr "Second string: "
                 second <- getLine
                 let lenFirst = length first
                     lenSecond = length second
                 putStrLn (show (max lenFirst lenSecond))

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \first =>
               putStr "Second string: " >>= \_ =>
               getLine >>= \second =>
               let lenFirst = length first
                   lenSecond = length second in
               putStrLn (show (max lenFirst lenSecond))
