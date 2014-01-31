{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

type Board = String

data PlayerState = PlayerState { energy :: Int, spawn :: Int }
    deriving (Show, Generic)

data GameState = GameState { rows :: Int , cols :: Int , p1 :: PlayerState
    , p2 :: PlayerState , grid :: Board , maxTurns :: Int , turnsElapsed :: Int }
    deriving (Show, Generic)

data GameInput = GameInput { state :: GameState, player :: Char }
             deriving (Show, Generic)

data GameMove = GameMove { from :: Int, to :: Int }
             deriving (Show, Generic)

type Space = (Int, Int)
type Energies = [Space]
type Enemies = [Space]
type PossibleSpaces = [Space]
type Row = Int
type Column = Int
type Rows = Int
type Columns = Int
type Weight = Double

data Move = Move { fromSpace:: Space, toSpace :: Space }
             deriving (Show, Generic)

instance FromJSON GameInput
instance ToJSON GameInput
instance FromJSON GameState
instance ToJSON GameState
instance FromJSON PlayerState
instance ToJSON PlayerState
instance FromJSON GameMove
instance ToJSON GameMove

isMyPiece :: Char -> Board -> Row -> Bool
isMyPiece p g i = p == g !! i

findEnergies :: Rows -> Columns -> Board -> Energies
findEnergies rs cs g = [(r, c) | r <- [1..rs], c <- [1..cs], isEnergy r c]
    where isEnergy r c = '*' == g !! (cs * (r - 1) + (c - 1))

findSpaces :: Rows -> Columns -> Board -> PossibleSpaces
findSpaces rs cs g = [(r, c) | r <- [1..rs], c <- [1..cs], isSpace r c]
    where isSpace r c = '.' == g !! (cs * (r - 1) + (c - 1))

findEnemies :: Rows -> Columns -> Board -> Char -> Enemies
findEnemies rs cs g p = [(r, c) | r <- [1..rs], c <- [1..cs], isEnemy r c]
    where
        e = if p == 'r' then 'b' else 'r'
        isEnemy r c = e == g !! (cs * (r - 1) + (c - 1))

distance :: Space -> Space -> Double
distance x y
    | snd x == snd y && fst x == fst y = 0
    | otherwise = sqrt $ dy + dx
        where
            dx = fromIntegral $ ((fst x - fst y) * (fst x - fst y))
            dy = fromIntegral $ ((snd x - snd y) * (snd x - snd y))

considerEnergy :: Space -> Energies -> [Weight]
considerEnergy p = map $ distance p

considerEnemies :: Space -> Enemies -> [Weight]
considerEnemies p = map $ distance p

considerMove :: Row -> Column -> Energies -> Enemies -> Space -> (Move, Weight)
considerMove r c ns es p =
    case distance (r, c) p of
        1 -> considerMove' r c ns es p
        _ -> (Move (r, c) p, 0.0)
    where
        considerMove' r c ns es p =
            (Move (r, c) p, enemyScore / energyScore)
            where
                energyScore = sum $ considerEnergy p ns
                enemyScore = sum $ considerEnemies p es

bestMove :: [(Move, Weight)] -> (Move, Weight)
bestMove = foldr1 (\a b -> if (snd a) > (snd b) then a else b)

makeMove :: Row -> Column -> Energies -> Enemies -> PossibleSpaces -> Move
makeMove r c ns es ps = fst $ bestMove $ map (considerMove r c ns es) ps

makeMoves :: Rows -> Columns -> Board -> Char -> [Move]
makeMoves rs cs g p = [makeMove r c ns es (ps ++ ns) | r <- [1..rs], c <- [1..cs], isMyPiece p g (cs * (r - 1) + (c - 1))]
    where
        ps = findSpaces rs cs g
        ns = findEnergies rs cs g
        es = findEnemies rs cs g p

moveToGameMove :: Rows -> Columns -> Move -> GameMove
moveToGameMove rs cs m = GameMove os ns
    where
        os = cs * ((fst $ fromSpace m) - 1) + (snd $ fromSpace m) - 1
        ns = cs * ((fst $ toSpace m) - 1) + (snd $ toSpace m) - 1

makeTurn :: GameInput -> [GameMove]
makeTurn g = map (moveToGameMove r c) $ makeMoves r c gr p
    where
        p = player g
        s = state g
        r = rows s
        c = cols s
        gr = grid s

main :: IO ()
main = do
  input <- getLine
  let req = decode $ BL.pack input :: Maybe GameInput
  case req of
    Just g -> putStrLn $ BL.unpack $ encode $ makeTurn g
    Nothing -> putStrLn "Error!"
