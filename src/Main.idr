module Main

import Conway.Conway
import Conway.Graphics
import Conway.RLE

import Effect.File

import Data.Vect

showCW : Conway m n -> String
showCW (MkConway v) = concat $ map (\row => showRow row ++ "\n") v
  where showRow : Vect n CellState -> String
        showRow v = concat $ map (\cs => case cs of
                            Alive => ". "
                            Dead  => "  "
                        ) v


loop : Conway m n -> IO ()
loop cw = do
       putStrLn $ showCW cw
       getLine
       loop $ iterateGame cw 

maxLength : List (List a) -> Nat
maxLength xs = maxLength' 0 xs
  where maxLength' : Nat -> List (List a) -> Nat
        maxLength' n [] = n
        maxLength' n (x::xs) = maxLength' (max n (length x)) xs


repeatN : (n : Nat) -> a -> Vect n a
repeatN Z _ = []
repeatN (S n) a = (a::(repeatN n a))

lToV : (n : Nat) -> List (List CellState) -> List (Vect n CellState)
lToV n [] = []
lToV n ys = map (\x => f n x) ys
  where f : (n : Nat) -> List CellState -> Vect n CellState
        f Z _ = []
        f (S n) [] = (Dead::(f n []))
        f (S n) (y::ys) = (y::(f n ys))



main : IO ()
main = do
  args <- getArgs
  let fNameM = index' 1 args
  case fNameM of
    Nothing => print "Usage ./Conway 'filename'"
    Just fName => do
        cwM <- readCFile fName 
        case cwM of 
          Nothing => print "Error in converting file contents\n"
          Just cw => do
            let lVec = lToV (maxLength cw) cw
            let vec = fromList lVec
            let conway = MkConway vec
            conwayLoop conway      
                  





 {-
  conwayLoop cw
  where
      row1 : Vect 4 CellState
      row1 = fromList [Dead, Alive, Dead, Dead]

      row2 : Vect 4 CellState
      row2 = fromList [Dead, Alive, Dead, Dead]
      
      row3 : Vect 4 CellState
      row3 = fromList [Dead, Alive, Dead, Dead]

      row4 : Vect 4 CellState
      row4 = fromList [Dead, Dead, Dead, Dead]

      v : Vect 4 (Vect 4 CellState) 
      v = fromList [row1, row2, row3, row4]

      cw : Conway 4 4 
      cw = MkConway v
  -}
