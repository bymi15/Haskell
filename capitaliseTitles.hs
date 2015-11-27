import Data.Char (toLower, toUpper)

capitalised :: String -> String
capitalised [] = []
capitalised (x:xs) = toUpper x : lowerTail xs
  where
     lowerTail [] = []
     lowerTail (x:xs) = toLower x : lowerTail xs 

lowercase :: String -> String
lowercase [] = []
lowercase (x:xs) = toLower x : lowercase xs

title :: [String] -> [String]
title (x:xs) = capitalised x : titleRemaining xs
   where
     titleRemaining  [] = []
     titleRemaining  (x:xs) | (length x) > 3 = capitalised x : titleRemaining xs
                            | otherwise      = lowercase x : titleRemaining xs
