module Main exposing (..)

import Debug exposing (toString)
import List exposing (concat, indexedMap, repeat)
import Playground exposing (..)
import String exposing (fromInt)


defaultSize : number
defaultSize =
    20


tileSize : number
tileSize =
    40


defaultBombCount : number
defaultBombCount =
    5


type alias Model =
    { tiles : TileMap }


initialModel : Model
initialModel =
    { tiles = repeat defaultSize (repeat defaultSize (Hidden Empty)) }


main =
    game view update initialModel


type TileContent
    = Bomb
    | Empty


type Tile
    = Revealed TileContent
    | Hidden TileContent


type alias TileMap =
    List (List Tile)


viewTile : Tile -> Shape
viewTile tile =
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    square black 40

                Empty ->
                    group
                        [ square grey 40
                        , words black (fromInt 1)
                        ]

        Hidden _ ->
            square darkGrey 40


viewTileAt : Int -> Int -> Tile -> Shape
viewTileAt i j tile =
    viewTile tile
        |> move (screenPosition i) (screenPosition j)


screenPosition : Int -> Float
screenPosition k =
    toFloat (((defaultSize // 2) - k) * (tileSize + 5))


view : Computer -> Model -> List Shape
view computer model =
    (words black (toString computer.mouse.x) |> move -300 -100)
        :: concat (tilesIndexedMap (\x y tile -> viewTileAt x y tile) model.tiles)


tilesIndexedMap : (Int -> Int -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (f x) tiles) tileMap


isHoveringTile : ( Float, Float ) -> ( Int, Int ) -> Bool
isHoveringTile ( x, y ) ( tileX, tileY ) =
    let
        screenX =
            screenPosition tileX

        screenY =
            screenPosition tileY
    in
    (((screenX - (tileSize / 2)) < x) && (x < (screenX + (tileSize / 2))))
        && (((screenY - (tileSize / 2)) < y) && (y < (screenY + (tileSize / 2))))


update : Computer -> Model -> Model
update computer memory =
    let
        clicked =
            computer.mouse.click

        mouseX =
            computer.mouse.x

        mouseY =
            computer.mouse.y
    in
    { tiles =
        tilesIndexedMap
            (\x y tile ->
                case tile of
                    Revealed _ ->
                        tile

                    Hidden c ->
                        if clicked && isHoveringTile ( mouseX, mouseY ) ( x, y ) then
                            Debug.log "Probar" (Revealed c)

                        else
                            tile
            )
            memory.tiles
    }
