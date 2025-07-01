module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (preventDefaultOn)
import Json.Decode
import List exposing (member)
import Random
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Task


defaultSize : number
defaultSize =
    10


tileSize : number
tileSize =
    45


tileSpacing : number
tileSpacing =
    3


defaultBombCount : number
defaultBombCount =
    10


type alias Screen =
    { height : Float, width : Float }


type alias TilePosition =
    ( Int, Int )


type alias ScreenPosition =
    { x : Float, y : Float }


type alias Model =
    { tiles : TileMap, screen : Screen, bombs : List TilePosition }



-- INIT


init : Model
init =
    { tiles = Dict.fromList (List.map (\p -> ( p, Hidden Empty )) positions)
    , bombs = []
    , screen = { height = 0, width = 0 }
    }


bombsGenerator : Random.Generator (List TilePosition)
bombsGenerator =
    Random.list defaultBombCount
        (Random.pair
            (Random.int 1 defaultSize)
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
    Dict TilePosition Tile


type Msg
    = MouseClick Float Float
    | RightClick Float Float
    | GotViewport Dom.Viewport
    | GeneratedBombs (List TilePosition)



-- VIEW


baseTile : ScreenPosition -> String -> Svg msg
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


neighbours : TilePosition -> List TilePosition
neighbours ( tileX, tileY ) =
    List.filter
        (\( x, y ) ->
            (0 <= x)
                && (x < defaultSize)
                && (0 <= y)
                && (y < defaultSize)
        )
        (List.concatMap
            (\n -> List.map (\m -> ( tileX + n, tileY + m )) [ -1, 0, 1 ])
            [ -1, 0, 1 ]
        )


tileNumber : TilePosition -> List TilePosition -> Int
tileNumber tilePosition bombs =
    List.length (List.filter (\p -> member p bombs) (neighbours tilePosition))


tileBombCount : TilePosition -> ScreenPosition -> List TilePosition -> Svg Msg
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
        [ text (String.fromInt (tileNumber tilePosition bombs)) ]


viewTile : Tile -> List TilePosition -> TilePosition -> ScreenPosition -> Svg Msg
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


boardOffset : Float -> Float
boardOffset screenSize =
    (screenSize / 2) - ((defaultSize / 2) * (tileSize + tileSpacing))


tileToScreenPosition : Int -> Float -> Float
tileToScreenPosition k screenSize =
    toFloat k * (tileSize + tileSpacing) + boardOffset screenSize


between : number -> number -> number -> Bool
between lo hi x =
    lo <= x && x < hi


screenToTileCoord : Float -> Float -> Maybe Int
screenToTileCoord k screenSize =
    let
        offset =
            boardOffset screenSize
    in
    if between offset (offset + screenSize) k then
        Just (round (k - offset) // (tileSize + tileSpacing))

    else
        Nothing


screenToTilePosition : ScreenPosition -> Screen -> Maybe TilePosition
screenToTilePosition position screen =
    let
        xTileCoord =
            screenToTileCoord position.x screen.width

        yTileCoord =
            screenToTileCoord position.y screen.height
    in
    Maybe.andThen (\x -> Maybe.andThen (\y -> Just ( x, y )) yTileCoord) xTileCoord


viewTileAt : Model -> TilePosition -> Maybe (Svg Msg)
viewTileAt { screen, bombs, tiles } position =
    Maybe.andThen
        (\tile ->
            Just
                (viewTile
                    tile
                    bombs
                    position
                    { x = tileToScreenPosition (Tuple.first position) screen.width
                    , y = tileToScreenPosition (Tuple.second position) screen.height
                    }
                )
        )
        (Dict.get position tiles)


positions : List TilePosition
positions =
    let
        coords =
            List.range 0 (defaultSize - 1)
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) coords) coords


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
            (List.filterMap (viewTileAt model) positions)
        ]


coordIsHoveringTile : Float -> Float -> Int -> Bool
coordIsHoveringTile aux mouseCoordinate tileCoordinate =
    let
        screenCoordinate =
            tileToScreenPosition tileCoordinate aux
    in
    (screenCoordinate <= mouseCoordinate) && (mouseCoordinate < (screenCoordinate + tileSize))



-- UPDATE


revealTile : Tile -> Tile
revealTile tile =
    case tile of
        Hidden c ->
            Revealed c

        Revealed _ ->
            tile

        Flagged _ ->
            tile


flagTile : Tile -> Tile
flagTile tile =
    case tile of
        Revealed _ ->
            tile

        Hidden c ->
            Flagged c

        Flagged c ->
            Hidden c


updateModelTiles : Model -> (Tile -> Tile) -> ScreenPosition -> Model
updateModelTiles model action mousePosition =
    let
        hoveredTilePosition =
            screenToTilePosition mousePosition model.screen
    in
    case hoveredTilePosition of
        Nothing ->
            model

        Just p ->
            { model
                | tiles =
                    Dict.update p
                        (Maybe.andThen (\tile -> Just (action tile)))
                        model.tiles
            }


revealTileAndMaybeNeighbours : Model -> TilePosition -> Model
revealTileAndMaybeNeighbours model position =
    let
        newModel =
            { model
                | tiles =
                    Dict.update position
                        (Maybe.andThen (\tile -> Just (revealTile tile)))
                        model.tiles
            }
    in
    if tileNumber position model.bombs > 0 then
        newModel

    else
        List.foldr
            (\x rec -> revealTileAndMaybeNeighbours rec x)
            newModel
            (List.filter
                (\p ->
                    case Dict.get p model.tiles of
                        Just (Hidden _) ->
                            True

                        _ ->
                            False
                )
                (neighbours position)
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseClick mouseX mouseY ->
            ( let
                hoveredTilePosition =
                    screenToTilePosition (ScreenPosition mouseX mouseY) model.screen
              in
              case hoveredTilePosition of
                Nothing ->
                    model

                Just p ->
                    revealTileAndMaybeNeighbours model p
            , Cmd.none
            )

        RightClick mouseX mouseY ->
            ( updateModelTiles model flagTile (ScreenPosition mouseX mouseY), Cmd.none )

        GotViewport { viewport } ->
            ( { model | screen = { height = viewport.height, width = viewport.width } }, Cmd.none )

        GeneratedBombs bombs ->
            ( { model
                | bombs = bombs
                , tiles =
                    List.foldr
                        (\bombPosition dict ->
                            Dict.update bombPosition
                                (Maybe.andThen
                                    (\tile ->
                                        Just
                                            (case tile of
                                                Revealed _ ->
                                                    Revealed Bomb

                                                Hidden _ ->
                                                    Hidden Bomb

                                                Flagged _ ->
                                                    Flagged Bomb
                                            )
                                    )
                                )
                                dict
                        )
                        model.tiles
                        bombs
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
