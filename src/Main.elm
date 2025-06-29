module Main exposing (..)

import Browser
import Browser.Events exposing (onClick)
import Debug exposing (toString)
import Html exposing (Html, div)
import Json.Decode as D
import List exposing (concat, indexedMap, repeat)
import String exposing (fromFloat, fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type TileContent
    = Bomb
    | Empty


type Position
    = Pos Float Float


type Tile
    = Revealed TileContent
    | Hidden TileContent


type alias TileMap =
    List (List Tile)


type Msg
    = MouseClick Float Float


baseTile : Position -> String -> Svg msg
baseTile (Pos xPos yPos) colorStr =
    let
        sTileSize =
            fromFloat tileSize
    in
    rect
        [ x (fromFloat xPos)
        , y (fromFloat yPos)
        , width sTileSize
        , height sTileSize
        , fill colorStr
        ]
        []


viewTile : Tile -> Position -> Svg Msg
viewTile tile position =
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    baseTile position "red"

                Empty ->
                    baseTile position "grey"

        Hidden _ ->
            baseTile position "darkGrey"


viewTileAt : Position -> Tile -> Svg Msg
viewTileAt (Pos i j) tile =
    viewTile tile (Pos (screenPosition i) (screenPosition j))


screenPosition : Float -> Float
screenPosition k =
    k * (tileSize + tileSpacing)


view : Model -> Html Msg
view model =
    svg
        [ width (fromFloat (defaultSize * (tileSize + tileSpacing)))
        , height (fromFloat (defaultSize * (tileSize + tileSpacing)))
        ]
        (concat (tilesIndexedMap (\p tile -> viewTileAt p tile) model.tiles))


tilesIndexedMap : (Position -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (\y -> f (Pos (toFloat x) (toFloat y))) tiles) tileMap


isHoveringTile : Position -> Position -> Bool
isHoveringTile (Pos mouseX mouseY) (Pos tileX tileY) =
    let
        screenX =
            screenPosition tileX

        screenY =
            screenPosition tileY
    in
    ((screenX <= mouseX) && (mouseX < (screenX + tileSize)))
        && ((screenY <= mouseY) && (mouseY < (screenY + tileSize)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        MouseClick mouseX mouseY ->
            { tiles =
                tilesIndexedMap
                    (\pos tile ->
                        case tile of
                            Revealed _ ->
                                tile

                            Hidden c ->
                                if isHoveringTile (Pos mouseX mouseY) pos then
                                    Revealed c

                                else
                                    tile
                    )
                    model.tiles
            }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onClick (D.map2 MouseClick (D.field "pageX" D.float) (D.field "pageY" D.float))
