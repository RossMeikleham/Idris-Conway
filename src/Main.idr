module Main

import Conway
import Graphics
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
main = conwayLoop cw
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
