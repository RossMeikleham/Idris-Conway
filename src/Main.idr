module Main

import Conway
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

main : IO ()
main = loop cw
  where
      row1 : Vect 3 CellState
      row1 = fromList [Dead, Alive, Dead]

      row2 : Vect 3 CellState
      row2 = fromList [Dead, Alive, Dead]
      
      row3 : Vect 3 CellState
      row3 = fromList [Dead, Alive, Dead]

      v : Vect 3 (Vect 3 CellState) 
      v = fromList [row1, row2, row3]

      cw : Conway 3 3 
      cw = MkConway v
