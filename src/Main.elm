module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Debug exposing (toString)
import Html exposing (Html, div)
import Json.Decode
import List exposing (concat, indexedMap, repeat)
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


defaultSize : number
defaultSize =
    15


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
    { tiles : TileMap, screenWidth : Float, screenHeight : Float }


init : Model
init =
    { tiles = repeat defaultSize (repeat defaultSize (Hidden Empty))
    , screenHeight = 0
    , screenWidth = 0
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Task.perform GotViewport Dom.getViewport )
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
    | GotViewport Dom.Viewport


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



screenPosition : Float -> Float -> Float
screenPosition k aux =
    k * (tileSize + tileSpacing) + ((aux / 2) - ((defaultSize / 2) * (tileSize + tileSpacing)))


viewTileAt : Model -> Position -> Tile -> Svg Msg
viewTileAt model (Pos i j) tile =
    viewTile tile (Pos (screenPosition i model.screenWidth) (screenPosition j model.screenHeight))


view : Model -> Html Msg
view model =
    svg
        [ viewBox ("0 0 " ++ String.fromFloat model.screenWidth ++ " " ++ String.fromFloat model.screenHeight)
        , width "100%"
        , height "100%"
        ]
        (concat (tilesIndexedMap (\p tile -> viewTileAt model p tile) model.tiles))


tilesIndexedMap : (Position -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (\y -> f (Pos (toFloat x) (toFloat y))) tiles) tileMap


isHoveringTile : Model -> Position -> Position -> Bool
isHoveringTile model (Pos mouseX mouseY) (Pos tileX tileY) =
    let
        screenX =
            screenPosition tileX model.screenWidth

        screenY =
            screenPosition tileY model.screenHeight
    in
    ((screenX <= mouseX) && (mouseX < (screenX + tileSize)))
        && ((screenY <= mouseY) && (mouseY < (screenY + tileSize)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        MouseClick mouseX mouseY ->
            { model
                | tiles =
                    tilesIndexedMap
                        (\pos tile ->
                            case tile of
                                Revealed _ ->
                                    tile

                                Hidden c ->
                                    if isHoveringTile model (Pos mouseX mouseY) pos then
                                        Revealed c

                                    else
                                        tile
                        )
                        model.tiles
            }

        GotViewport { viewport } ->
            { model
                | screenHeight = viewport.height
                , screenWidth = viewport.width
            }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onClick
        (Json.Decode.map2 MouseClick
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )
