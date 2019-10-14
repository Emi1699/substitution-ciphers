{-
  @uthor: Buliga Fanel Emanuel
  #: ASSIGNMENT 1, 
-}

module Homework2 where
	import Data.Char
	import AssignmentHelp

	-- myCipher :: Cipher
	-- myCipher = "EKMFLGDQVZNTOWYHKUSPAIBRCJ"

	validateCipher :: Cipher -> Bool
	validateCipher cipher = length cipher == 26 && charsUniq cipher

	charsUniq :: String -> Bool
	charsUniq cipher
		| length cipher == 1 = True -- the character must be unique is it's the only one in the list
		| elem (head cipher) (tail cipher) = False -- if the character's present in multiple places in the list, then it's not unique
		| otherwise = charsUniq (tail cipher) -- check recursively if the remaining elements are unique

	encode :: Cipher -> Int -> Char -> Char
	encode cipher offset character
		| alphaPos character == alphaPos (currentChr cipher) = head cipher
		| otherwise = encode (tail cipher) offset character

	{- 
	 because the mapping is static, we're always moving through both the alphabet and the cipher at the same rate
	 so, 'currentChr cipher' will return the letter of the alphabet that corresponds to the character we're currently at in the cipher
	 I felt this clarification might be needed in order to not confuse future me when looking over the code
	-}
	currentChr :: Cipher -> Char
	currentChr cipher = chr (91 - length cipher)




