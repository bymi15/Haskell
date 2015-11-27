filterMe :: (a -> Bool) -> [a] -> [a]
filterMe _ [] = []
filterMe p (x:xs) | p x        = x : filterMe p xs
                  | otherwise  = filterMe p xs

retrieveCapitals :: String -> String
retrieveCapitals xs = filterMe (`elem` ['A'..'Z']) xs
