{-
  @uthor: Buliga Fanel Emanuel
  #: ASSIGNMENT 1, 
-}

module Homework2 where
	import Data.Char
	import AssignmentHelp

	myCipher :: Cipher
	myCipher = "EKMFLGDQVZNTOWYHKUSPAIBRCJ"

	validateCipher :: Cipher -> Bool
	validateCipher cipher = length cipher == 26 && charsUniq cipher


	charsUniq :: String -> Bool
	charsUniq cipher
		| length cipher == 1 = True -- the character must be unique is it's the only one in the list
		| elem (head cipher) (tail cipher) = False -- if the character's present in multiple places in the list, then it's not unique
		| otherwise = charsUniq (tail cipher) -- check recursively if the remaining elements are unique


	encode :: Cipher -> Int -> Char -> Char
	encode cipher offset character
		| (alphaPos character - offset) `mod` 26 == (alphaPos (currentAlphabetChr cipher)) `mod` 26 = head cipher 
		| otherwise = encode (tail cipher) offset character
		

	encodeMessage :: Cipher -> Int -> String -> String
	encodeMessage cipher offset [] = []
	encodeMessage cipher offset (x:xs) = encode cipher offset x : encodeMessage cipher offset xs


	reverseEncode :: Cipher -> Int -> Char -> Char
	reverseEncode cipher offset encChar
		| encChar == head cipher = chr (((alphaPos (currentAlphabetChr cipher) + offset) `mod` 26) + ord 'A')
		| otherwise = reverseEncode (tail cipher) offset encChar



	reverseEncodeMessage :: Cipher -> Int -> String -> String
	reverseEncodeMessage cipher offset [] = []
	reverseEncodeMessage cipher offset (x:xs) = reverseEncode cipher offset x : reverseEncodeMessage cipher offset xs
		


	letterStats :: String -> [(Char, Int)]
	letterStats message = mergesort cmp [(chr, p) | chr <- ['A'..'Z'], let p = percent (length (filter (==chr) message)) (length message), p > 0]


	partialDecode :: [(Char, Char)] -> String -> String
	partialDecode guesses message
		| length guesses > 0 = partialDecode (tail guesses) (map (\x -> if x == snd (head guesses) then toLower (fst (head guesses)); else x) message)
		| otherwise = message


	-- helper functions

	cmp :: (Char, Int) -> (Char, Int) -> Bool
	cmp t1 t2 
		| snd t1 < snd t2 = False
		| otherwise = True

	myGuesses = [('E', 'W'), ('T', 'J'), ('G', 'L'), ('Y', 'Z'), ('P', 'T'), ('V', 'H'), ('S', 'A'), ('O', 'F'), ('I', 'Q'), ('N', 'X'), ('S', 'E'), ('H', 'C'), ('R', 'Y'), ('D', 'N'), ('L', 'P'), ('C', 'M'), ('U', 'B'), ('P', 'V'), ('W', 'D'), ('F', 'R')]
	
	{- 
	 we're always moving through both the alphabet and the cipher at the same rate
	 so, 'currentChr cipher' will return the letter of the alphabet that corresponds to the character we're currently at in the cipher
	 I felt this clarification might be needed in order to not confuse future me when looking over the code
	-}
	currentAlphabetChr :: String -> Char
	currentAlphabetChr cipher = chr (91 - length cipher)



