rotor :: (Show a) => Int -> [a] -> [a]
rotor y xs | (y < 0 || y > length xs)  = error "Offset number out of bounds"
           | otherwise = (drop y xs) ++ (take y xs)

makeKey :: Int -> [(Char,Char)]
alphabet = ['A'..'Z']
makeKey x = zip alphabet (rotor x alphabet)

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = c
lookUp c ((key,val):pairs) | c == key    = val
                           | otherwise   = lookUp c pairs

encipher :: Int -> Char -> Char
encipher x c = lookUp c (makeKey x)

normalise :: String -> String
normalise [] = []
normalise (x:xs)  | isAlpha x    = toUpper x : normalise xs
                  | isDigit x    = x : normalise xs
                  | otherwise    = normalise xs

encipherStr :: Int -> String -> String
encipherStr x str = [encipher x c | c <- normalise str]

swapKeys :: [(Char, Char)] -> [(Char, Char)]
swapKeys pairs = [(b, a) | (a, b) <- pairs]

decipher :: Int -> Char -> Char
decipher x c = lookUp c (swapKeys (makeKey x))

decipherStr :: Int -> String -> String
decipherStr x str = [decipher x c | c <- str]

bruteforce :: Int -> String -> [String]
bruteforce x str | x > 0     = decipherStr x str : (bruteforce (x-1) str)
                 | otherwise = []

decrypt :: String -> [String]
decrypt str = bruteforce 26 str
