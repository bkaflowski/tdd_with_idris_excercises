palindrome : Nat -> String -> Bool
palindrome n str = let longEnough = (length str) > n 
                   in longEnough && (isPalindrome str)
    where
      isPalindrome : String -> Bool
      isPalindrome s = (toLower s) == (toLower (reverse s))

showPalindrome : String -> String
showPalindrome str = "Word is palindrome: " ++ show (palindrome 0 str) ++ "\n"

main : IO ()
main = repl "> " showPalindrome