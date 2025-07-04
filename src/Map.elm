module Map exposing
    ( Size
    , TileMap
    , emptyTileMap
    , neighbours
    , positions
    , sizeFromDifficulty
    , tileBombCount
    )

import Dict exposing (Dict)
import Menu
import Tile
    exposing
        ( Content(..)
        , Position
        , Tile(..)
        )
import Utils exposing (listCount)


type alias TileMap =
    Dict Position Tile


type alias Size =
    ( Int, Int )


positions : Size -> List Position
positions ( width, height ) =
    let
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


emptyTileMap : Size -> Dict Position Tile
emptyTileMap size =
    Dict.fromList (List.map (\p -> ( p, Hidden Empty )) (positions size))


between : number -> number -> number -> Bool
between lo hi x =
    lo <= x && x < hi


neighbours : ( Int, Int ) -> Position -> List Position
neighbours ( width, height ) ( tileX, tileY ) =
    List.filter
        (\( x, y ) ->
            between 0 width x
                && between 0 height y
                && (( x, y ) /= ( tileX, tileY ))
        )
        (List.concatMap
            (\n -> List.map (\m -> ( tileX + n, tileY + m )) [ -1, 0, 1 ])
            [ -1, 0, 1 ]
        )


tileBombCount : List Position -> Size -> (Position -> Int)
tileBombCount bombs size tilePosition =
    listCount (\p -> List.member p bombs) (neighbours size tilePosition)
