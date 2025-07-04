module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes as HtmlAttr
import Random
import StartScreen
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Tile


defaultBombCount : number
defaultBombCount =
    Tile.defaultSize


type alias GameState =
    { tiles : TileMap, bombs : List Tile.TilePosition }


type Model
    = StartScreen StartScreen.Model
    | Playing GameState
    | Lost GameState



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
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias TileMap =
    Dict Tile.TilePosition Tile.Tile


type Msg
    = StartGame
    | RevealTile Tile.TilePosition
    | FlagTile Tile.TilePosition
    | GeneratedBombs (List Tile.TilePosition)



-- VIEW


viewTileAt : GameState -> Tile.TilePosition -> Maybe (Svg Msg)
viewTileAt { bombs, tiles } position =
    Maybe.andThen
        (\tile ->
            Just
                (Tile.viewTile
                    tile
                    bombs
                    position
                    |> Svg.map
                        (\m ->
                            case m of
                                Tile.RevealTile p ->
                                    RevealTile p

                                Tile.FlagTile p ->
                                    FlagTile p
                        )
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
    div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-items" "center"
        ]
        [ h1 [ HtmlAttr.style "fontFamily" "monospace" ] [ text "Minesweeper" ]
        , case model of
            StartScreen _ ->
                StartScreen.view StartScreen.Waiting |> Html.map (\_ -> StartGame)

            Playing gameState ->
                svg
                    [ SvgAttr.width (String.fromInt Tile.screenSize)
                    , SvgAttr.height (String.fromInt Tile.screenSize)
                    , SvgAttr.viewBox
                        ("0 0 "
                            ++ String.fromInt Tile.screenSize
                            ++ " "
                            ++ String.fromInt Tile.screenSize
                        )
                    ]
                    (List.filterMap (viewTileAt gameState) positions)

            Lost gameState ->
                div
                    [ HtmlAttr.style "display" "flex"
                    , HtmlAttr.style "flex-direction" "column"
                    , HtmlAttr.style "align-items" "center"
                    ]
                    [ svg
                        [ SvgAttr.width (String.fromInt Tile.screenSize)
                        , SvgAttr.height (String.fromInt Tile.screenSize)
                        , SvgAttr.viewBox
                            ("0 0 "
                                ++ String.fromInt Tile.screenSize
                                ++ " "
                                ++ String.fromInt Tile.screenSize
                            )
                        , SvgAttr.opacity "0.5"
                        ]
                        (List.filterMap (viewTileAt gameState) positions)
                    , span
                        [ HtmlAttr.style "font-size" "20px"
                        , HtmlAttr.style "font" "monospace"
                        , HtmlAttr.style "margin-bottom" "10px"
                        ]
                        [ text "Perdiste" ]
                    ]
        ]



-- UPDATE


updateGameTiles : GameState -> (Tile.Tile -> Tile.Tile) -> Tile.TilePosition -> GameState
updateGameTiles game action position =
    { game
        | tiles =
            Dict.update position
                (Maybe.andThen (\tile -> Just (action tile)))
                game.tiles
    }


isEmptyWithNoNeighbourBombs : GameState -> Tile.TilePosition -> Bool
isEmptyWithNoNeighbourBombs game position =
    (Tile.tileNumber position game.bombs == 0)
        && (Dict.get position game.tiles == Just (Tile.Hidden Tile.Empty))


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
        StartScreen _ ->
            case msg of
                StartGame ->
                    ( Playing
                        { tiles = Dict.fromList (List.map (\p -> ( p, Tile.Hidden Tile.Empty )) positions), bombs = [] }
                    , Random.generate GeneratedBombs bombsGenerator
                    )

                _ ->
                    ( model, Cmd.none )

        Playing game ->
            case msg of
                StartGame ->
                    ( model, Cmd.none )

                RevealTile p ->
                    if List.member p game.bombs then
                        ( Lost (revealTileAndMaybeNeighbours game p), Cmd.none )

                    else
                        ( Playing (revealTileAndMaybeNeighbours game p), Cmd.none )

                FlagTile p ->
                    ( Playing (updateGameTiles game Tile.flagTile p), Cmd.none )

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

        Lost _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
