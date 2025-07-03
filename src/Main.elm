module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onClick)
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (preventDefaultOn)
import Json.Decode
import Random
import Screen exposing (Screen, ScreenPosition)
import StartScreen
import Svg exposing (..)
import Svg.Attributes as SvgAttr
import Task
import Tile


tileSpacing : number
tileSpacing =
    1


defaultBombCount : number
defaultBombCount =
    Tile.defaultSize


type alias GameState =
    { tiles : TileMap, screen : Screen, bombs : List Tile.TilePosition }


type Model
    = StartScreen StartScreen.Model
    | Playing GameState



-- INIT


init : Model
init =
    StartScreen StartScreen.Waiting


bombsGenerator : Random.Generator (List Tile.TilePosition)
bombsGenerator =
    Random.list defaultBombCount
        (Random.pair
            (Random.int 1 Tile.defaultSize)
            (Random.int 1 Tile.defaultSize)
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


type alias TileMap =
    Dict Tile.TilePosition Tile.Tile


type Msg
    = StartGame
    | MouseClick Float Float
    | RightClick Float Float
    | GotViewport Dom.Viewport
    | GeneratedBombs (List Tile.TilePosition)



-- VIEW


boardOffset : Float -> Float
boardOffset screenSize =
    (screenSize / 2) - ((Tile.defaultSize / 2) * (Tile.tileSize + tileSpacing))


tileToScreenPosition : Int -> Float -> Float
tileToScreenPosition k screenSize =
    toFloat k * (Tile.tileSize + tileSpacing) + boardOffset screenSize


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
        Just (round (k - offset) // (Tile.tileSize + tileSpacing))

    else
        Nothing


screenToTilePosition : ScreenPosition -> Screen -> Maybe Tile.TilePosition
screenToTilePosition position screen =
    let
        xTileCoord =
            screenToTileCoord position.x screen.width

        yTileCoord =
            screenToTileCoord position.y screen.height
    in
    Maybe.andThen (\x -> Maybe.andThen (\y -> Just ( x, y )) yTileCoord) xTileCoord


viewTileAt : GameState -> Tile.TilePosition -> Maybe (Svg Msg)
viewTileAt { screen, bombs, tiles } position =
    Maybe.andThen
        (\tile ->
            Just
                (Tile.viewTile
                    tile
                    bombs
                    position
                    { x = tileToScreenPosition (Tuple.first position) screen.width
                    , y = tileToScreenPosition (Tuple.second position) screen.height
                    }
                )
        )
        (Dict.get position tiles)


positions : List Tile.TilePosition
positions =
    let
        coords =
            List.range 0 (Tile.defaultSize - 1)
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) coords) coords


view : Model -> Html Msg
view model =
    case model of
        StartScreen _ ->
            StartScreen.view StartScreen.Waiting |> Html.map (\_ -> StartGame)

        Playing gameState ->
            div
                [ onRightClick
                , HtmlAttr.style "width" "100vw"
                , HtmlAttr.style "height" "100vh"
                ]
                [ svg
                    [ SvgAttr.viewBox
                        ("0 0 "
                            ++ String.fromFloat gameState.screen.width
                            ++ " "
                            ++ String.fromFloat gameState.screen.height
                        )
                    , SvgAttr.width "100%"
                    , SvgAttr.height "100%"
                    ]
                    (List.filterMap (viewTileAt gameState) positions)
                ]



-- UPDATE


updateGameTiles : GameState -> (Tile.Tile -> Tile.Tile) -> ScreenPosition -> GameState
updateGameTiles game action mousePosition =
    let
        hoveredTilePosition =
            screenToTilePosition mousePosition game.screen
    in
    case hoveredTilePosition of
        Nothing ->
            game

        Just p ->
            { game
                | tiles =
                    Dict.update p
                        (Maybe.andThen (\tile -> Just (action tile)))
                        game.tiles
            }


isEmptyWithNoNeighbourBombs : GameState -> Tile.TilePosition -> Bool
isEmptyWithNoNeighbourBombs game position =
    (Tile.tileNumber position game.bombs == 0)
        && (Dict.get position game.tiles == Just (Tile.Hidden Tile.Empty))



-- FIXME: ugly


revealTileAndMaybeNeighbours : GameState -> Tile.TilePosition -> GameState
revealTileAndMaybeNeighbours game position =
    let
        newModel =
            { game
                | tiles =
                    Dict.update position
                        (Maybe.andThen (\tile -> Just (Tile.revealTile tile)))
                        game.tiles
            }
    in
    if isEmptyWithNoNeighbourBombs game position then
        List.foldr
            (\x rec -> revealTileAndMaybeNeighbours rec x)
            newModel
            (List.filter
                (\p ->
                    case Dict.get p game.tiles of
                        Just (Tile.Hidden _) ->
                            True

                        _ ->
                            False
                )
                (Tile.neighbours position)
            )

    else
        newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        StartScreen m ->
            case msg of
                StartGame ->
                    ( Playing
                        { tiles = Dict.fromList (List.map (\p -> ( p, Tile.Hidden Tile.Empty )) positions)
                        , bombs = []
                        , screen = { height = 0, width = 0 }
                        }
                    , Cmd.batch
                        [ Task.perform GotViewport Dom.getViewport
                        , Random.generate GeneratedBombs bombsGenerator
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Playing game ->
            case msg of
                StartGame ->
                    ( model, Cmd.none )

                MouseClick mouseX mouseY ->
                    ( let
                        hoveredTilePosition =
                            screenToTilePosition (ScreenPosition mouseX mouseY) game.screen
                      in
                      case hoveredTilePosition of
                        Nothing ->
                            Playing game

                        Just p ->
                            Playing (revealTileAndMaybeNeighbours game p)
                    , Cmd.none
                    )

                RightClick mouseX mouseY ->
                    ( Playing (updateGameTiles game Tile.flagTile (ScreenPosition mouseX mouseY)), Cmd.none )

                GotViewport { viewport } ->
                    ( Playing { game | screen = { height = viewport.height, width = viewport.width } }, Cmd.none )

                GeneratedBombs bombs ->
                    ( Playing
                        { game
                            | bombs = bombs
                            , tiles =
                                List.foldr
                                    (\bombPosition dict ->
                                        Dict.update bombPosition
                                            (Maybe.andThen
                                                (\tile -> Just (Tile.putBomb tile))
                                            )
                                            dict
                                    )
                                    game.tiles
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
subscriptions _ =
    onClick
        (Json.Decode.map2 MouseClick
            (Json.Decode.field "pageX" Json.Decode.float)
            (Json.Decode.field "pageY" Json.Decode.float)
        )
