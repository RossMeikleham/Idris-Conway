module Graphics

import Data.Vect
import Graphics.SDL
import Conway


-- | Dimensions of SDL surface (x, y)
screenDims : (Int, Int)
screenDims = (640, 480)

-- | Performs a sequence of IO actions
sequenceIO : List (IO ()) -> IO ()
sequenceIO Nil = return ()
sequenceIO (x::xs) =
  do x
     sequenceIO xs

-- | Converts fin to int
finToInt : Fin m -> Int
finToInt = toIntNat . finToNat

-- | Display the current conway state on the given SDL surface
displayConway : Conway m n -> SDLSurface -> IO ()
displayConway {m} {n} (MkConway v) s = 
  do 
     sequenceIO $ toList $ map displayRow (sequenceFin m) 
     flipBuffers s

    where 
        iterY : Int
        iterY = (snd screenDims) `div` (toIntNat m)

        iterX : Int
        iterX = (fst screenDims) `div` (toIntNat n)

        drawCell : Int -> Int -> (Int, Int, Int) -> IO ()
        drawCell py px (r,g,b) = filledRect s (iterX * px) (iterY * py) iterX iterY r g b 128

        displayRow : Fin m -> IO ()
        displayRow y = sequenceIO $ toList $ map displayPoint (sequenceFin n)

        where displayPoint : Fin n -> IO ()
              displayPoint x with (index x (index y v))
                    | Alive = drawCell (finToInt y) (finToInt x) (0x00, 0x00, 0x00)
                    | Dead  = drawCell (finToInt y) (finToInt x) (0xFF, 0xFF, 0xFF)


-- | Given an initial Conway State, constantly updates 
--   and displays the Conway State
conwayLoop : Conway m n -> IO ()
conwayLoop cw = 
  do 
    surface <- startSDL (fst screenDims) (snd screenDims)
    conwayLoop' cw surface

  where conwayLoop' : Conway m n -> SDLSurface -> IO ()
        processEvent : Conway m n -> SDLSurface -> Maybe Event -> IO (Maybe (Conway m n))
        
        
        conwayLoop' cw surface = do
            displayConway cw surface
            flipBuffers surface
            event <- pollEvent
            eventPResult <- processEvent cw surface event

            case eventPResult of
              Just nextCw => conwayLoop' nextCw surface
              Nothing     => return ()


        processEvent cw surface (Just (KeyDown KeyEnter)) 
                = do
                    let nextCw = iterateGame cw
                    displayConway nextCw surface
                    flipBuffers surface
                    return $ Just nextCw 
                   

        processEvent _ _ (Just AppQuit) 
                = return Nothing

        processEvent cw _ _ 
                = return $ Just cw
