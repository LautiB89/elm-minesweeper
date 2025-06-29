module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import List exposing (concat, indexedMap, repeat)
import String exposing (fromInt)
import Browser
import Html exposing (Html)
import Html exposing (div)

defaultSize : number
defaultSize =
    10


tileSize : number
tileSize =
    40

tileSpacing : number
tileSpacing =
    3

defaultBombCount : number
defaultBombCount =
    5


type alias Model =
    { tiles : TileMap }


initialModel : Model
initialModel =
    { tiles = repeat defaultSize (repeat defaultSize (Hidden Empty)) }

main =
      Browser.document
    { init = initialModel
    , view = view
    , update = update
    , subscriptions = \_ -> {}
    }


type TileContent
    = Bomb
    | Empty


type Tile
    = Revealed TileContent
    | Hidden TileContent


type alias TileMap =
    List (List Tile)

type Msg = Alo
viewTile : Tile -> (Int, Int) -> Svg
viewTile tile (xPos, yPos) =
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    rect
                        [ x xPos
                        , y yPos
                        , width tileSize
                        , height tileSize
                        , rx "15"
                        , ry "15"
                        ]
                        []
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
    viewTile tile (screenPosition i) (screenPosition j)


screenPosition : Int -> Float
screenPosition k =
    toFloat (((defaultSize // 2) - k) * (tileSize + tileSpacing))


view : Model -> Html msg
view model =
    svg (concat (tilesIndexedMap (\x y tile -> viewTileAt x y tile) model.tiles))


tilesIndexedMap : (Int -> Int -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (f x) tiles) tileMap


isHoveringTile : Mouse -> ( Int, Int ) -> Bool
isHoveringTile mouse ( tileX, tileY ) =
    let

        mouseX = mouse.x

        mouseY = mouse.y
        screenX =
            screenPosition tileX

        screenY =
            screenPosition tileY
    in
    (((screenX - (tileSize / 2)) < mouseX) && (mouseX < (screenX + (tileSize / 2))))
        && (((screenY - (tileSize / 2)) < mouseY) && (mouseY < (screenY + (tileSize / 2))))


update : Computer -> Model -> Model
update computer memory =
    { tiles =
        tilesIndexedMap
            (\x y tile ->
                case tile of
                    Revealed _ ->
                        tile

                    Hidden c ->
                        if computer.mouse.click && isHoveringTile computer.mouse ( x, y ) then
                            Revealed c

                        else
                            tile
            )
            memory.tiles
    }
