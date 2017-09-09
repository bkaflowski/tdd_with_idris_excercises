module Main

import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses: Nat) -> IO ()
guess target k = do putStrLn ("Number of tries: " ++ show k)
                    putStr "Guess number: "
                    Just input <- readNumber
                        | Nothing => do putStrLn "NaN"
                                        guess target (S k)
                    case compare input target of
                         LT => do putStrLn "Number too small"
                                  guess target (S k)
                         GT => do putStrLn "Number too big"
                                  guess target (S k)
                         EQ => putStrLn "Congrats! That's the number."


generateNrToGuess : IO (Nat)
generateNrToGuess = do timeValue <- time
                       let nrToGuess = mod timeValue 100
                       putStrLn ("Number to guess: " ++ show nrToGuess)
                       pure (fromIntegerNat nrToGuess)

main : IO()
main = do target <- generateNrToGuess
          guess target 0

repl2 : (prompt : String) -> (onInput : String -> String) -> IO ()
repl2 prompt onInput = do putStr prompt
                          input <- getLine
                          putStrLn (onInput input)
                          repl2 prompt onInput

replWith2 : (state : a) -> (prompt : String) ->
    (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith2 state prompt onInput =
        do putStr prompt
           input <- getLine
           case onInput state input of
                Just (output, newState) => do putStr output
                                              replWith2 newState prompt onInput
                Nothing => pure ()


testRepl2 : IO ()
testRepl2 = repl2 "Put string: " (\x => "Input string was: " ++ show x)

func : Integer -> String -> Maybe (String, Integer)
func sum input = if all isDigit (unpack input)
                    then let result = (cast input) + sum in
                         Just ("Sum: " ++ show result ++ "\n", result)
                    else Nothing

testReplWith : IO ()
testReplWith =
  replWith 0 "Add number: " func

testReplWith2 : IO()
testReplWith2 =
  replWith2 0 "Add number: " func
