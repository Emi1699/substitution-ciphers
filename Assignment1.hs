{-
  @uthor: Buliga Fanel Emanuel
-}

module Assignment1 where
	import Data.Char
	import AssignmentHelp

	validateCipher :: Cipher -> Bool
	validateCipher cipher = length cipher == 26 && charsUniq cipher
	-- tested with the cipher in the assignment file (EKMFLGDQVZNTOWYHXUSPAIBRCJ)


	charsUniq :: String -> Bool
	charsUniq cipher
		| length cipher == 1 = True
		| elem (head cipher) (tail cipher) = False
		| otherwise = charsUniq (tail cipher)
	-- tested in validateCipher

	encode :: Cipher -> Int -> Char -> Char
	encode cipher offset character
		| (alphaPos character - offset) `mod` 26 == (alphaPos (currentAlphabetChr cipher)) `mod` 26 = head cipher 
		| otherwise = encode (tail cipher) offset character
		

	-- encodes each letter at a time, recursively
	encodeMessage :: Cipher -> Int -> String -> String
	encodeMessage cipher offset [] = []
	encodeMessage cipher offset (x:xs) = encode cipher offset x : encodeMessage cipher offset xs

	-- opposite of encode
	reverseEncode :: Cipher -> Int -> Char -> Char
	reverseEncode cipher offset encChar
		| encChar == head cipher = chr (((alphaPos (currentAlphabetChr cipher) + offset) `mod` 26) + ord 'A')
		| otherwise = reverseEncode (tail cipher) offset encChar


	-- same as encodeMessage
	reverseEncodeMessage :: Cipher -> Int -> String -> String
	reverseEncodeMessage cipher offset [] = []
	reverseEncodeMessage cipher offset (x:xs) = reverseEncode cipher offset x : reverseEncodeMessage cipher offset xs
	-- tested with "SHEWDTLS"
	-- !! all the encode and decode functions were tested with the cipher in the assignment file 
	-- !! all the tests turned out as expected


	-- uses list comprehension to build a list made of tuples containing the characters contained in a message
	-- + their frequency in said message
	-- sorted in descending order of frequency
	letterStats :: String -> [(Char, Int)]
	letterStats message = mergesort cmpInt [(chr, p) | chr <- ['A'..'Z'], let p = percent (length (filter (==chr) message)) (length message), p > 0]
	-- tested with "STDDWSD", worked as expected


	-- 
	partialDecode :: [(Char, Char)] -> String -> String
	partialDecode guesses message
		| length guesses > 0 = partialDecode (tail guesses) (map (\x -> if x == snd (head guesses) then toLower (fst (head guesses)); else x) message)
		| otherwise = message
	-- tried to decode the mystery message by using a list of guesses and got this:
	-- QJadXayJrkwdXZXankaJQJnJQrzpQhmdwhwrsQcdcyrnmXsdXxrzvdzrnvmtdaaXvdaJrhxdJatXZdJmQarzdXxQJJxdkQJxrzvdwaJrhrZQJamrnxckdJmdwQvmJarwJrjaQbdzreaJrhtXykdzrJxdJaQzpwdXadJmdtdaaXvdxdzvJmXkQJtrwdaJrhZddhJmQatdaaXvdadpwdJrwamXwdQjyrneXzJJmdemrxdpxXaaJrvdJJmdkrznatXwZaaJrh
	-- don't really know how to proceed further



	---- Helper functions ----

	-- used in letterStats
	cmpInt :: (Char, Int) -> (Char, Int) -> Bool       
	cmpInt t1 t2                                             
		| snd t1 < snd t2 = False                            
		| otherwise = True                                   
          								
	-- used to sort engFreq 			
	cmpFloat :: (Char, Float) -> (Char, Float) -> Bool
	cmpFloat t1 t2 					
		| snd t1 < snd t2 = False          
		| otherwise = True		
								
	-- used in buildGuesses   
	cmpChar :: (Char, Char) -> (Char, Char) -> Bool          
	cmpChar t1 t2          		
		| snd t1 > snd t2 = False   
		| otherwise = True	


 	-- i know the guesses are supposed to be introduced by hand, this is just to make my work easier
	-- creates an ordered list of guesses for the letters in the mystery message based on the frequency of the letters in the english alphabet
	-- used in decoding the mystery message
	-- plot twist: couldn't do it
	buildGuesses :: [(Char, Float)] -> [(Char, Int)] -> [(Char, Char)]
	buildGuesses engStats msgStats 
		| length msgStats > 0 && length engStats > 0 = mergesort cmpChar ((toUpper (fst (head msgStats)), toUpper (fst (head engStats))) : buildGuesses (tail engStats) (tail msgStats))
		| otherwise = []

	-- we're always moving through both the alphabet and the cipher at the same rate
	-- 'currentAlphabetChr cipher' will return the letter of the alphabet that corresponds to the character we're currently at in the cipher
	currentAlphabetChr :: String -> Char
	currentAlphabetChr cipher = chr (91 - length cipher)





