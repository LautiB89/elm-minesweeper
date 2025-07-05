module TileMap exposing
    ( Size
    , TileMap
    , anyIsRevealed
    , countBombNeighbours
    , empty
    , get
    , positions
    , revealNonFlaggedNeighbours
    , revealTileAndMaybeNeighbours
    , revealedTiles
    , sizeFromDifficulty
    , totalTiles
    , update
    )

import Dict exposing (Dict)
import Menu
import Tile exposing (Position, Tile)
import Utils exposing (listCount)


type alias Size =
    ( Int, Int )


type alias TileMap =
    { tiles : Dict Position Tile, size : Size }


empty : Menu.GameDifficulty -> TileMap
empty difficulty =
    let
        size : Size
        size =
            sizeFromDifficulty difficulty
    in
    { tiles = Dict.fromList (List.map (\p -> ( p, Tile.hiddenEmpty )) (positions size))
    , size = size
    }


positions : Size -> List Position
positions ( width, height ) =
    let
        coords : Int -> List Int
        coords =
            \size -> List.range 0 (size - 1)
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) (coords height)) (coords width)


sizeFromDifficulty : Menu.GameDifficulty -> ( Int, Int )
sizeFromDifficulty difficulty =
    case difficulty of
        Menu.Easy ->
            ( 9, 9 )

        Menu.Medium ->
            ( 16, 16 )

        Menu.Hard ->
            ( 30, 16 )


isValid : Size -> Position -> Bool
isValid ( width, height ) ( x, y ) =
    (0 <= x && x < width)
        && (0 <= y && y < height)


neighbours : Size -> Position -> List Position
neighbours size ( tileX, tileY ) =
    List.filter
        (\( x, y ) -> isValid size ( x, y ) && (( x, y ) /= ( tileX, tileY )))
        (List.concatMap
            (\n -> List.map (\m -> ( tileX + n, tileY + m )) [ -1, 0, 1 ])
            [ -1, 0, 1 ]
        )


filterNeighbours : (Tile -> Bool) -> TileMap -> Position -> List Position
filterNeighbours p tileMap position =
    List.filter
        (\n ->
            get n tileMap
                |> Maybe.map p
                |> Maybe.withDefault False
        )
        (neighbours tileMap.size position)


countBombNeighbours : TileMap -> Position -> Int
countBombNeighbours =
    countNeighbours Tile.hasBomb


countFlaggedNeighbours : TileMap -> Position -> Int
countFlaggedNeighbours =
    countNeighbours Tile.isFlagged


countNeighbours : (Tile -> Bool) -> TileMap -> Position -> Int
countNeighbours p tileMap centralPosition =
    listCount
        (\position ->
            get position tileMap
                |> Maybe.map p
                |> Maybe.withDefault False
        )
        (neighbours tileMap.size centralPosition)


satisfiesAt : (Tile -> Bool) -> Position -> TileMap -> Bool
satisfiesAt p position tileMap =
    case get position tileMap of
        Just tile ->
            p tile

        Nothing ->
            False


get : Position -> TileMap -> Maybe Tile
get position { size, tiles } =
    if isValid size position then
        Dict.get position tiles

    else
        Nothing


update : Position -> (Maybe Tile -> Maybe Tile) -> TileMap -> TileMap
update position f tileMap =
    if isValid tileMap.size position then
        { tileMap | tiles = Dict.update position f tileMap.tiles }

    else
        tileMap


count : (Position -> Tile -> Bool) -> TileMap -> Int
count p tileMap =
    Dict.size (Dict.filter p tileMap.tiles)


revealedTiles : TileMap -> Int
revealedTiles tileMap =
    count
        (\_ tile -> not (Tile.isHidden tile) && not (Tile.hasBomb tile))
        tileMap


revealNonFlaggedNeighbours : TileMap -> Position -> TileMap
revealNonFlaggedNeighbours tileMap position =
    if isRevealedWithAllFlags tileMap position then
        List.foldr (\x rec -> revealTileAndMaybeNeighbours rec x)
            tileMap
            (List.filter
                (\p -> satisfiesAt Tile.isHidden p tileMap)
                (neighbours tileMap.size position)
            )

    else
        tileMap


revealTileAndMaybeNeighbours : TileMap -> Position -> TileMap
revealTileAndMaybeNeighbours tileMap position =
    let
        newTileMap : TileMap
        newTileMap =
            update position (Maybe.map Tile.reveal) tileMap

        isEmptyWithNoNeighbourBombs : Bool
        isEmptyWithNoNeighbourBombs =
            satisfiesAt (\tile -> Tile.isHidden tile && Tile.isEmpty tile) position tileMap
                && (countBombNeighbours tileMap position == 0)
    in
    if isEmptyWithNoNeighbourBombs then
        List.foldr
            (\x rec -> revealTileAndMaybeNeighbours rec x)
            newTileMap
            (filterNeighbours Tile.isHidden tileMap position)

    else
        newTileMap


isRevealedWithAllFlags : TileMap -> Position -> Bool
isRevealedWithAllFlags tileMap position =
    (countBombNeighbours tileMap position == countFlaggedNeighbours tileMap position)
        && satisfiesAt (\tile -> Tile.isRevealed tile && Tile.isEmpty tile) position tileMap


anyIsRevealed : List Position -> TileMap -> Bool
anyIsRevealed tilePositions tileMap =
    List.any
        (\p -> satisfiesAt (\tile -> Tile.isRevealed tile && Tile.hasBomb tile) p tileMap)
        tilePositions


totalTiles : TileMap -> Int
totalTiles tileMap =
    let
        ( width, height ) =
            tileMap.size
    in
    width * height
