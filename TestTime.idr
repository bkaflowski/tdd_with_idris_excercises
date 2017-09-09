import System

testTime : IO ()
testTime = do timeValue <- time
              putStrLn (show (mod timeValue 100))

testRepl : IO ()
testRepl = repl "Put string: " (\x => "Input string was: " ++ show x)
