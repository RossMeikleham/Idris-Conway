-- Reads Runtime Length Encoded files
-- containing a Game of Life
module Conway.RLE

import Conway.Conway

-- map all uppercase to lowercase
lower : (List String) -> (List String)
lower = map toLower

-- Remove whitespace
removeWS : (List String) -> (List String)
removeWS = map (\s => pack $ filter (\c => not $ isSpace c) (unpack s)) 

-- Remove Comments from RLE
removeComments : (List String) -> (List String)
removeComments = filter (\s => not $ isEmptyOrComment $ unpack s) 
  where isEmptyOrComment : List Char -> Bool
        isEmptyOrComment [] = True
        isEmptyOrComment (x::xs) = x == '#'
        

compress : (List String) -> (List String)
compress [] = []
compress (x::xs) = x::[concat xs]


-- Functions for Parsing a String into an Int
charToInt : Char -> Maybe Int
charToInt c = let i = cast {to=Int} c in
              let zero = cast {to=Int} '0' in
              let nine = cast {to=Int} '9' in
              if i < zero || i > nine
                then Nothing
                else Just (i - zero)

total
parse' : Int -> List Int -> Maybe Int
parse' _   []      = Nothing
parse' acc [d]     = Just (10 * acc + d)
parse' acc (d::ds) = parse' (10 * acc + d) ds


total parseInt : String -> Maybe Int
parseInt str = (sequence (map charToInt (unpack str))) >>= parse' 0



-- Obtain dimensions x * y of Conway board
getDimensions : (List String) -> Maybe (Int, Int)
getDimensions [] = Nothing
getDimensions (x::xs) = getDim x
  where getDim : String -> Maybe (Int, Int)
        getDim s = case (pack $ take 2 $ unpack s) of
              "x=" => do 
                        x <- parseInt (pack $ takeWhile (/= ',') (drop 2 $ unpack s))
                        case (pack $ take 3 s2) of
                          ",y=" => do 
                              y <- parseInt (pack $ drop 3 s2)
                              return (x, y)
                          _ => Nothing
              _ => Nothing
  
        where
            s2 : List Char 
            s2 = dropWhile (/= ',') $ unpack s



getInitial : Int -> (List String) -> Maybe (List (List CellState))
getInitial _ [] = Nothing
getInitial _ (x1::[]) = Nothing
getInitial y (x1::x2::xs) = getInitial' y (unpack x2)
  where     
        next : (List Char) -> Maybe (List Char)
        next xs = case dropWhile (/= '$') xs of
                    [] => Nothing
                    (x::xs) => Just xs


        getN : (List Char) -> Maybe (Nat, List Char)
        getN xs = case (takeWhile isDigit) xs of
                    [] => Just (1, xs)
                    digits => do 
                      n <- parseInt (pack digits)
                      return (cast n, drop (length digits) xs)

        repeatN : Nat -> a -> List a
        repeatN Z a = []
        repeatN (S n) a = a::(repeatN n a)

        getLine : (List Char) -> Maybe (List (CellState))
        getLine ('$'::xs) = Just []
        getLine xs = do
          numNext <- getN xs
          case (snd numNext) of
            ('b'::xs) => liftA (\l => (repeatN (fst numNext) Dead) ++ l)  (getLine xs)
            ('o'::xs) => liftA (\l => (repeatN (fst numNext) Alive) ++ l) (getLine xs)
            _        => Nothing


        getInitial' : Int  -> (List Char) -> Maybe (List (List CellState))
        getInitial' 0 _ = Just []
        getInitial' y xs = do
            first <- getLine xs
            nextItems <- next xs
            rest <- getInitial' (y - 1) nextItems 
            return (first::rest)


getConway : List (String) -> Maybe (List (List CellState))
getConway lines = do
  dimensions <- getDimensions lines
  getInitial (snd dimensions) lines


readCFile : String -> IO (Maybe (List (List CellState))) 
readCFile fName = do
  str <- readFile fName
  let strLines = lines str
  let noWSLines = compress $ removeWS $ removeComments $ strLines
  return $ getConway noWSLines  
