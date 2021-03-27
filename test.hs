stringAvailable :: Char -> Bool
stringAvailable x 
    | "\"" = True
    | "\r" = True
    | "\n" = True
    | "\t" = True
    | otherwise    = False

