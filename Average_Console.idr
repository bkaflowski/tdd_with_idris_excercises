module Main

import Average

showAverage : String -> String
showAverage str = "The average word length is: " ++ (show (avg_word_length str)) ++ "\n"
main : IO ()
main = repl "Enter a string: " showAverage