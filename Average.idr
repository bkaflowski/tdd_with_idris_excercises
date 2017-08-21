module Average

export
avg_word_length : String -> Double
avg_word_length sentence = let lettersTotal = lettersCount sentence 
                               wordsNr = wordsCount sentence 
                           in cast lettersTotal / cast wordsNr
where
	wordsCount : String -> Nat
	wordsCount str = length (words str)
	
	lettersCount : String -> Nat
	lettersCount str = sum (map length (words str))