module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Debug exposing (toString)
import Html exposing (Html, div)
import Json.Decode
import List exposing (concat, filter, foldr, indexedMap, length, member, repeat)
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
    20


type alias Screen =
    { height : Float, width : Float }


type alias Model =
    { tiles : TileMap, screen : Screen, bombs : List Position }


init : Model
init =
    { tiles = repeat defaultSize (repeat defaultSize (Hidden Empty))
    , bombs = []
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


neighbours : Position -> List Position
neighbours (Pos x y) =
    List.concatMap
        (\n ->
            List.map (\m -> Pos (x + n) (y + m)) [ -1, 0, 1 ]
        )
        [ -1, 0, 1 ]


tileBombCount : Position -> Position -> List Position -> Svg Msg
tileBombCount tilePosition (Pos screenXPos screenYPos) bombs =
    text_
        [ x (String.fromFloat (screenXPos + (tileSize / 2)))
        , y (String.fromFloat (screenYPos + (tileSize / 2)))
        , textAnchor "middle"
        , dominantBaseline "central"
        , fontSize "16"
        , fontFamily "monospace"
        , fill "white"
        ]
        [ text (String.fromInt (List.length (List.filter (\p -> member p bombs) (neighbours tilePosition)))) ]


viewTile : Tile -> List Position -> Position -> Position -> Svg Msg
viewTile tile bombs tilePosition screenPosition =
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    baseTile screenPosition "red"

                Empty ->
                    g [] [ baseTile screenPosition "grey", tileBombCount tilePosition screenPosition bombs ]

        Hidden _ ->
            baseTile screenPosition "darkGrey"


tileToScreenPosition : Float -> Float -> Float
tileToScreenPosition k aux =
    k * (tileSize + tileSpacing) + ((aux / 2) - ((defaultSize / 2) * (tileSize + tileSpacing)))


viewTileAt : Model -> Position -> Tile -> Svg Msg
viewTileAt { screen, bombs } (Pos i j) tile =
    viewTile tile
        bombs
        (Pos i j)
        (Pos
            (tileToScreenPosition i screen.width)
            (tileToScreenPosition j screen.height)
        )


viewTiles : Model -> List (Svg Msg)
viewTiles model =
    concat (tilesIndexedMap (\p tile -> viewTileAt model p tile) model.tiles)


view : Model -> Html Msg
view model =
    svg
        [ viewBox
            ("0 0 "
                ++ String.fromFloat model.screen.width
                ++ " "
                ++ String.fromFloat model.screen.height
            )
        , width "100%"
        , height "100%"
        ]
        (viewTiles model)


tilesIndexedMap : (Position -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap (\x tiles -> indexedMap (\y -> f (Pos (toFloat x) (toFloat y))) tiles) tileMap


isHoveringTile : Screen -> Position -> Position -> Bool
isHoveringTile screen (Pos mouseX mouseY) (Pos tileX tileY) =
    let
        screenX =
            tileToScreenPosition tileX screen.width

        screenY =
            tileToScreenPosition tileY screen.height
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
                                        Hidden Bomb

                            else
                                tile
                        )
                        model.tiles
                , bombs = List.map (\( x, y ) -> Pos (toFloat x) (toFloat y)) bombs
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
