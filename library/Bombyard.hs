module Bombyard (main) where


import Data.Function ((&))

import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import qualified Graphics.Gloss.Data.ViewPort as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Safe
import qualified System.Random as Random
import qualified System.Random.Shuffle as Shuffle


display :: Gloss.Display
display = let
    name = "Bombyard"

    width = 512
    height = 512
    size = (width, height)

    x = 0
    y = 0
    position = (x, y)

    in Gloss.InWindow name size position


opaque :: Int
opaque = 0xff


background :: Gloss.Color
background = let
    red = 0xff
    green = 0xff
    blue = 0xff

    in Gloss.makeColorI red green blue opaque


fps :: Int
fps = 60


data Visibility
    = Obscured
    | Visible
    deriving (Eq, Show)


data Cell
    = Bomb Visibility
    | Empty Visibility Int
    deriving (Show)


showCell :: Cell -> Cell
showCell cell = case cell of
    Bomb Obscured -> Bomb Visible
    Empty Obscured x -> Empty Visible x
    _ -> cell


data Status
    = Playing
    | Won
    | Lost
    deriving (Show)


data State = State
    { stateBombs :: Int
    , stateCells :: [[Cell]]
    , stateSeed :: Int
    , stateSize :: Int
    , stateStatus :: Status
    } deriving (Show)


defaultState :: State
defaultState = State
    { stateBombs = 0
    , stateCells = []
    , stateSeed = 0
    , stateSize = 0
    , stateStatus = Playing
    }


isBomb :: Cell -> Bool
isBomb cell = case cell of
    Bomb _ -> True
    Empty _ _ -> False


bombsAround :: (Int, Int) -> [[Cell]] -> Int
bombsAround (x, y) cells = let
    bombs =
        [ cell
        | iy <- [y - 1, y, y + 1]
        , ix <- [x - 1, x, x + 1]
        , iy /= y || ix /= x
        , row <- iy & Safe.atMay cells & Maybe.maybeToList
        , cell <- ix & Safe.atMay row & Maybe.maybeToList
        , isBomb cell
        ]
    in bombs & length


makeState :: Int -> Int -> Int -> State
makeState bombs seed size = let
    minSize = 2
    safeSize = max size minSize
    area = safeSize * safeSize

    minBombs = 1
    maxBombs = area - 1
    safeBombs = bombs & min maxBombs & max minBombs

    generator = Random.mkStdGen seed
    bombCells = Bomb Obscured & replicate safeBombs
    emptyCells = Empty Obscured 0 & repeat
    cells = bombCells ++ emptyCells & take area
    shuffledCells = Shuffle.shuffle' cells area generator
    gridCells = Split.chunksOf safeSize shuffledCells

    newCells = gridCells
        & zip [0 ..]
        & map (\ (y, row) -> row
            & zip [0 ..]
            & map (\ (x, cell) -> case cell of
                Empty visibility _ -> let
                    n = bombsAround (x, y) gridCells
                    in Empty visibility n
                Bomb _ -> cell))

    in defaultState
        { stateBombs = safeBombs
        , stateCells = newCells
        , stateSeed = seed
        , stateSize = safeSize
        }


data World = World
    { worldSize :: (Int, Int)
    , worldState :: State
    , worldStep :: Int
    , worldTime :: Float
    } deriving (Show)


defaultWorld :: World
defaultWorld = World
    { worldSize = (0, 0)
    , worldState = defaultState
    , worldStep = 0
    , worldTime = 0
    }


cellVisibility :: Cell -> Visibility
cellVisibility cell = case cell of
    Bomb visibility -> visibility
    Empty visibility _ -> visibility


drawCell :: Cell -> Gloss.Picture
drawCell cell = case cellVisibility cell of
    Obscured -> Gloss.pictures
        [ Gloss.rectangleSolid 2 2
            & Gloss.color (Gloss.greyN 0.8)
        , Gloss.rectangleWire 2 2
        ]
    Visible -> case cell of
        Bomb _ -> Gloss.circleSolid 1
        Empty _ x -> if x == 0
            then Gloss.blank
            else x
                & show
                & Gloss.text
                & Gloss.scale 0.01 0.01
                & Gloss.translate (-0.4) (-0.5)


indexedCells :: State -> [((Int, Int), Cell)]
indexedCells state
    = state
    & stateCells
    & zip [0 ..]
    & concatMap (\ (y, row) -> row
        & zip [0 ..]
        & map (\ (x, cell) -> ((x, y), cell)))


translateCell :: State -> Int -> Int -> Gloss.Picture -> Gloss.Picture
translateCell state x y cell = let
    size = state & stateSize & fromIntegral
    scale = 2 / size
    offset = (size - 1) / size
    translate a = a * scale - offset
    x' = x & fromIntegral & translate
    y' = y & fromIntegral & translate
    in Gloss.translate x' y' cell


scaleCell :: State -> Gloss.Picture -> Gloss.Picture
scaleCell state cell = let
    x = 1 / fromIntegral (stateSize state)
    y = x
    in Gloss.scale x y cell


drawCells :: State -> Gloss.Picture
drawCells state
    = state
    & indexedCells
    & map (\ ((x, y), cell) -> cell
        & drawCell
        & scaleCell state
        & translateCell state x y)
    & Gloss.pictures


toViewPort :: World -> Gloss.ViewPort
toViewPort world = let
    (width, height) = worldSize world
    size = min width height
    scale = fromIntegral size / 2
    in Gloss.viewPortInit { Gloss.viewPortScale = scale }


draw :: World -> Gloss.Picture
draw world = let
    state = worldState world
    picture = case stateStatus state of
        Playing -> drawCells state
        Won -> Gloss.pictures
            [ drawCells state
            , "you won"
                & Gloss.text
                & Gloss.scale 0.003 0.003
                & Gloss.translate (-0.8) (-0.1)
                & Gloss.color Gloss.green
            ]
        Lost -> Gloss.pictures
            [ drawCells state
            , "you lost"
                & Gloss.text
                & Gloss.scale 0.003 0.003
                & Gloss.translate (-0.8) (-0.1)
                & Gloss.color Gloss.red
            ]
    viewPort = toViewPort world
    in Gloss.applyViewPortToPicture viewPort picture


getCellAt :: Int -> Int -> State -> Maybe Cell
getCellAt ix iy state = do
    let cells = stateCells state
    row <- Safe.atMay cells iy
    Safe.atMay row ix


setCellAt :: Int -> Int -> Cell -> State -> State
setCellAt ix iy newCell state = let
    newCells = state
        & stateCells
        & zip [0 ..]
        & map (\ (y, row) -> if iy /= y
            then row
            else row
                & zip [0 ..]
                & map (\ (x, cell) -> if ix /= x
                    then cell
                    else newCell))
    in state { stateCells = newCells }


isVisible :: Cell -> Bool
isVisible cell = cellVisibility cell == Visible


updateStatus :: State -> State
updateStatus state = case stateStatus state of
    Playing -> let
        cells = state & stateCells & concat
        bombRevealed = cells & filter isVisible & any isBomb
        allButBombsRevealed = cells & filter (not . isBomb) & all isVisible
        newStatus =
            if bombRevealed
            then Lost
            else if allButBombsRevealed
            then Won
            else stateStatus state
        in state { stateStatus = newStatus }
    _ -> state


clickOn' :: (Int, Int) -> World -> World
clickOn' (ix, iy) world = let
    state = worldState world
    in case getCellAt ix iy state of
        Just cell -> case cellVisibility cell of
            Obscured -> let
                newCell = showCell cell
                newState = setCellAt ix iy newCell state
                newerState = updateStatus newState
                in world { worldState = newerState }
            Visible -> world
        Nothing -> world


clickOn :: (Float, Float) -> World -> World
clickOn position world = case world & worldState & stateStatus of
    Playing -> let
        viewPort = toViewPort world
        (x, y) = Gloss.invertViewPort viewPort position

        state = worldState world
        size = state & stateSize & fromIntegral
        scale = size / 2
        translate a = floor ((a + 1) * scale)
        ix = translate x
        iy = translate y

        in clickOn' (ix, iy) world
    _ -> let
        state = worldState world
        seed = stateSeed state
        newSeed = succ seed -- ha ha
        bombs = stateBombs state
        size = stateSize state
        newState = makeState bombs newSeed size
        in world { worldState = newState }


handleKey :: Gloss.Key
    -> Gloss.KeyState
    -> Gloss.Modifiers
    -> (Float, Float)
    -> World
    -> World
handleKey key state _modifiers position world = case key of
    Gloss.MouseButton Gloss.LeftButton -> case state of
        Gloss.Up -> clickOn position world
        _ -> world
    _ -> world


handleMotion :: (Float, Float) -> World -> World
handleMotion _position world = world


handleResize :: (Int, Int) -> World -> World
handleResize size world = world { worldSize = size }


handle :: Gloss.Event -> World -> World
handle event world = case event of
    Gloss.EventKey key state modifiers position ->
        handleKey key state modifiers position world
    Gloss.EventMotion position ->
        handleMotion position world
    Gloss.EventResize size ->
        handleResize size world


step :: Float -> World -> World
step delta world = world
    { worldStep = worldStep world + 1
    , worldTime = worldTime world + delta
    }


main :: IO ()
main = do
    let Gloss.InWindow _name size _position = display

    seed <- Random.randomIO
    let state = makeState 10 seed 10

    let world = defaultWorld
            { worldSize = size
            , worldState = state
            }

    Gloss.play display background fps world draw handle step
