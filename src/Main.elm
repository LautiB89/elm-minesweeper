module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Debug exposing (toString)
import Html exposing (Html, div)
import Json.Decode
import List exposing (concat, indexedMap, member, repeat)
import Random
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


type alias Screen =
    { height : Float, width : Float }


type alias Model =
    { tiles : TileMap, screen : Screen }


init : Model
init =
    { tiles = repeat defaultSize (repeat defaultSize (Hidden Empty))
    , screen = { height = 0, width = 0 }
    }


bombsGenerator : Random.Generator (List ( Int, Int ))
bombsGenerator =
    Random.list defaultBombCount
        (Random.pair (Random.int 1 defaultSize)
            (Random.int 1 defaultSize)
        )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( init
                , Cmd.batch
                    [ Task.perform GotViewport Dom.getViewport
                    , Random.generate GeneratedBombs bombsGenerator
                    ]
                )
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
    | GeneratedBombs (List ( Int, Int ))


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


viewTileAt : Screen -> Position -> Tile -> Svg Msg
viewTileAt screen (Pos i j) tile =
    viewTile tile
        (Pos
            (screenPosition i screen.width)
            (screenPosition j screen.height)
        )


view : Model -> Html Msg
view { screen, tiles } =
    svg
        [ viewBox
            ("0 0 "
                ++ String.fromFloat screen.width
                ++ " "
                ++ String.fromFloat screen.height
            )
        , width "100%"
        , height "100%"
        ]
        (concat (tilesIndexedMap (\p tile -> viewTileAt screen p tile) tiles))


tilesIndexedMap : (Position -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (\y -> f (Pos (toFloat x) (toFloat y))) tiles) tileMap


isHoveringTile : Screen -> Position -> Position -> Bool
isHoveringTile screen (Pos mouseX mouseY) (Pos tileX tileY) =
    let
        screenX =
            screenPosition tileX screen.width

        screenY =
            screenPosition tileY screen.height
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
                                    if isHoveringTile model.screen (Pos mouseX mouseY) pos then
                                        Revealed c

                                    else
                                        tile
                        )
                        model.tiles
            }

        GotViewport { viewport } ->
            { model | screen = { height = viewport.height, width = viewport.width } }

        GeneratedBombs bombs ->
            { model
                | tiles =
                    tilesIndexedMap
                        (\(Pos x y) tile ->
                            if member ( round x, round y ) bombs then
                                case tile of
                                    Revealed _ ->
                                        Revealed Bomb

                                    Hidden _ ->
                                        Revealed Bomb

                            else
                                tile
                        )
                        model.tiles
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
