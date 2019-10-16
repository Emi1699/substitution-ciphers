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
		| (alphaPos character - offset) `mod` 26 == (alphaPos (currentAlphabetChr cipher)) `mod` 26 = head cipher 
		| otherwise = encode (tail cipher) offset character

	encodeMessage :: Cipher -> Int -> String -> String
	encodeMessage cipher offset message
		| length message > 0 = encode cipher offset (head message) : encodeMessage cipher offset (tail message)
		| otherwise = [] -- doesn't work without this guard???


	reverseEncode :: Cipher -> Int -> Char -> Char
	reverseEncode cipher offset encChar
		| encChar == head cipher = chr (((alphaPos (currentAlphabetChr cipher) + offset) `mod` 26) + ord 'A')
		| otherwise = reverseEncode (tail cipher) offset encChar

	reverseEncodeMessage :: Cipher -> Int -> String -> String
	reverseEncodeMessage cipher offset encMessage
		| length encMessage > 0 = reverseEncode cipher offset (head encMessage) : reverseEncodeMessage cipher offset (tail encMessage)
		| otherwise = []


	letterStats :: String -> [(Char, Int)]
	letterStats message = [(chr, p) | chr <- ['A'..'Z'], let p = percent (length (filter (==chr) message)) (length message), p > 0]

	{- 
	 we're always moving through both the alphabet and the cipher at the same rate
	 so, 'currentChr cipher' will return the letter of the alphabet that corresponds to the character we're currently at in the cipher
	 I felt this clarification might be needed in order to not confuse future me when looking over the code
	-}
	currentAlphabetChr :: String -> Char
	currentAlphabetChr cipher = chr (91 - length cipher)



