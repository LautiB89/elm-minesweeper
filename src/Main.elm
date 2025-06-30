module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (preventDefaultOn)
import Json.Decode
import List exposing (concat, indexedMap, member, repeat)
import Random
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Task


defaultSize : number
defaultSize =
    15


tileSize : number
tileSize =
    50


tileSpacing : number
tileSpacing =
    3


defaultBombCount : number
defaultBombCount =
    20


type alias Screen =
    { height : Float, width : Float }


type alias Position =
    { x : Float, y : Float }


type alias Model =
    { tiles : TileMap, screen : Screen, bombs : List Position }



-- INIT


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


type Tile
    = Revealed TileContent
    | Hidden TileContent
    | Flagged TileContent


type alias TileMap =
    List (List Tile)


type Msg
    = MouseClick Float Float
    | RightClick Float Float
    | GotViewport Dom.Viewport
    | GeneratedBombs (List ( Int, Int ))



-- VIEW


baseTile : Position -> String -> Svg msg
baseTile position colorStr =
    let
        sTileSize =
            fromFloat tileSize
    in
    rect
        [ SvgAttr.x (fromFloat position.x)
        , SvgAttr.y (fromFloat position.y)
        , SvgAttr.width sTileSize
        , SvgAttr.height sTileSize
        , SvgAttr.fill colorStr
        ]
        []


neighbours : Position -> List Position
neighbours position =
    List.concatMap
        (\n -> List.map (\m -> { x = position.x + n, y = position.y + m }) [ -1, 0, 1 ])
        [ -1, 0, 1 ]


tileBombCount : Position -> Position -> List Position -> Svg Msg
tileBombCount tilePosition screenPosition bombs =
    text_
        [ SvgAttr.x (String.fromFloat (screenPosition.x + (tileSize / 2)))
        , SvgAttr.y (String.fromFloat (screenPosition.y + (tileSize / 2)))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "central"
        , SvgAttr.fontSize (String.fromFloat (tileSize / 2))
        , SvgAttr.fontFamily "monospace"
        , SvgAttr.fill "white"
        , SvgAttr.pointerEvents "none"
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
                    g []
                        [ baseTile screenPosition "grey"
                        , tileBombCount tilePosition screenPosition bombs
                        ]

        Flagged _ ->
            baseTile screenPosition "brown"

        Hidden _ ->
            baseTile screenPosition "darkGrey"


tileToScreenPosition : Float -> Float -> Float
tileToScreenPosition k screenSize =
    k * (tileSize + tileSpacing) + ((screenSize / 2) - ((defaultSize / 2) * (tileSize + tileSpacing)))


viewTileAt : Model -> Position -> Tile -> Svg Msg
viewTileAt { screen, bombs } tilePosition tile =
    viewTile tile
        bombs
        tilePosition
        { x = tileToScreenPosition tilePosition.x screen.width
        , y = tileToScreenPosition tilePosition.y screen.height
        }


view : Model -> Html Msg
view model =
    div
        [ onRightClick
        , HtmlAttr.style "width" "100vw"
        , HtmlAttr.style "height" "100vh"
        ]
        [ svg
            [ SvgAttr.viewBox
                ("0 0 "
                    ++ String.fromFloat model.screen.width
                    ++ " "
                    ++ String.fromFloat model.screen.height
                )
            , SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (concat (tilesIndexedMap (viewTileAt model) model.tiles))
        ]


tilesIndexedMap : (Position -> Tile -> b) -> TileMap -> List (List b)
tilesIndexedMap f tileMap =
    indexedMap
        (\x tiles ->
            indexedMap (\y -> f { x = toFloat x, y = toFloat y }) tiles
        )
        tileMap


coordIsHoveringTile : Float -> Float -> Int -> Bool
coordIsHoveringTile aux mouseCoordinate tileCoordinate =
    let
        screenCoordinate =
            tileToScreenPosition (toFloat tileCoordinate) aux
    in
    (screenCoordinate <= mouseCoordinate) && (mouseCoordinate < (screenCoordinate + tileSize))



-- UPDATE


transformClickedTileBy : Model -> Float -> Float -> (Tile -> Tile) -> TileMap
transformClickedTileBy model mouseX mouseY t =
    indexedMap
        (\x tiles ->
            if coordIsHoveringTile model.screen.width mouseX x then
                indexedMap
                    (\y tile ->
                        if coordIsHoveringTile model.screen.height mouseY y then
                            t tile

                        else
                            tile
                    )
                    tiles

            else
                tiles
        )
        model.tiles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        MouseClick mouseX mouseY ->
            { model
                | tiles =
                    transformClickedTileBy model
                        mouseX
                        mouseY
                        (\tile ->
                            case tile of
                                Hidden c ->
                                    Revealed c

                                _ ->
                                    tile
                        )
            }

        RightClick mouseX mouseY ->
            { model
                | tiles =
                    transformClickedTileBy model
                        mouseX
                        mouseY
                        (\tile ->
                            case tile of
                                Revealed _ ->
                                    tile

                                Hidden c ->
                                    Flagged c

                                Flagged c ->
                                    Hidden c
                        )
            }

        GotViewport { viewport } ->
            { model | screen = { height = viewport.height, width = viewport.width } }

        GeneratedBombs bombs ->
            { model
                | tiles =
                    tilesIndexedMap
                        (\{ x, y } tile ->
                            if member ( round x, round y ) bombs then
                                case tile of
                                    Revealed _ ->
                                        Revealed Bomb

                                    Hidden _ ->
                                        Hidden Bomb

                                    Flagged _ ->
                                        Flagged Bomb

                            else
                                tile
                        )
                        model.tiles
                , bombs = List.map (\( x, y ) -> { x = toFloat x, y = toFloat y }) bombs
            }
    , Cmd.none
    )



-- SUBSCRIPTIONS


onRightClick : Html.Attribute Msg
onRightClick =
    preventDefaultOn "contextmenu"
        (Json.Decode.map2 (\x y -> ( RightClick x y, True ))
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    onClick
        (Json.Decode.map2 MouseClick
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )
