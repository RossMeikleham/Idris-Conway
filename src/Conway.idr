-- Conway's Game Of Life in Idris
module Conway

import Data.Vect

-- | An individual Cell can either be alive or dead
data CellState = Alive | Dead

data Conway : Nat -> Nat -> Type where 
   MkConway : (Vect m (Vect n CellState)) -> Conway m n

-- | Neighbours of a given cell, typically has 8 neighbours 
--   (we'll count the missing neighbours from edges as dead)
data Neighbours : Nat -> Type where
   MkNeighbours : CellState -> (Vect n CellState) -> Neighbours n


-- | Count number of alive neighbours, guaranteed to be <= total
--   number of neighbours
numAlive : Neighbours n -> Fin (n + 1)
numAlive {n} nbs = case natToFin (numAlive' nbs) (n + 1) of
                  Just f => f
  where
    -- | Count number of alive neighbours
    numAlive' : Neighbours n -> Nat
    numAlive' (MkNeighbours _ v) = foldl accFn 0 v
      where accFn : Nat -> CellState -> Nat
            accFn cnt Alive = (cnt + 1)
            accFn cnt Dead =  (cnt + 0) 


-- | Count the number of dead neighbours, guaranteed to be <= total
--   number of given neighbours
numDead : Neighbours n -> Fin (n + 1)
-- Dead  = Total - Alive
numDead {n} nb = case natToFin (n - (finToNat $ numAlive nb)) (n + 1) of 
                    Just f => f 


-- | Work out the new state of a cell given its neighbours
newCellState : Neighbours n -> CellState
newCellState (MkNeighbours cs v) with (cs, finToInteger $ numAlive $ MkNeighbours cs v)
  |  (Alive, 2) = Alive -- Stays alive
  |  (_, 3) = Alive -- Comes to life or stays alive
  |  (_, _) = Dead -- < 2 dies or stays dead and > 3 dies or stays dead
  


-- | Generate Neighbours for a given point in the board
getNeighbours : Conway m n -> Fin m -> Fin n -> Neighbours 8
getNeighbours (MkConway v) y x =  
  (MkNeighbours cs [l, topL, top, topR, r, botR, bot, botL])

  where
        -- Check if Cell at given position is Dead Or Alive
        -- If out of bounds the Cell is considered Dead
        getDoA : Conway a b -> Integer -> Integer -> CellState
        getDoA {a} {b} (MkConway v) y x  = 
          case getCell of 
            Just c => c
            Nothing => Dead

           where getCell : Maybe CellState
                 getCell = do
                    row <- integerToFin y a
                    col <- integerToFin x b
                    return $ index col (index row v)
        
        ix : Integer
        ix = finToInteger x
        iy : Integer
        iy = finToInteger y
        
        cs = index x (index y v)

        l    = getDoA (MkConway v)  iy      (ix - 1)
        topL = getDoA (MkConway v) (iy - 1) (ix - 1)
        top  = getDoA (MkConway v) (iy - 1)  ix
        topR = getDoA (MkConway v) (iy - 1) (ix + 1)
        r    = getDoA (MkConway v)  iy      (ix + 1)
        botR = getDoA (MkConway v) (iy + 1) (ix + 1)
        bot  = getDoA (MkConway v) (iy + 1)  ix
        botL = getDoA (MkConway v) (iy + 1) (ix - 1)


-- | Generates a List of values 0 to N - 1
--   Guarantees that returned Vector has N values                
sequence : (n : Nat) -> Vect n Nat
sequence n = reverse $ sequence' n
  where sequence' : (m : Nat) -> Vect m Nat
        sequence' Z = Nil 
        sequence' (S n) = n::(sequence' n)


-- | Generates a Vector of values 0 to N - 1
--   Guarantees that returned Vector has N values
--   and every values is < N
sequenceFin : (n : Nat) -> Vect n (Fin n)
sequenceFin n = map n2f (sequence n)
  where n2f : Nat -> Fin n
        n2f m = case natToFin m n of
                  Just f => f

-- | Get the next state from the current state
iterateGame : Conway m n -> Conway m n
iterateGame {m} {n} (MkConway v) =  MkConway (map iterateRow (sequenceFin m))
  where iterateRow : Fin m -> Vect n CellState
        iterateRow y = map iteratePos (sequenceFin n)
        where iteratePos : Fin n -> CellState
              iteratePos x = newCellState (getNeighbours (MkConway v) y x)     
