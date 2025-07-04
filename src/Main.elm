module Main exposing (GameState, Model(..), Msg(..), main)

import Browser
import Dict
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Map exposing (Size, TileMap, emptyTileMap, tileBombCount)
import Menu
import Random
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Tile exposing (Content(..), Position, Tile(..), viewTile)
import Utils exposing (listNub)


type alias GameState =
    { size : ( Int, Int ), tiles : TileMap, bombs : List Position }


type Model
    = MainMenu Menu.Model
    | Playing GameState
    | Lost GameState
    | Won GameState



-- INIT


init : Model
init =
    MainMenu Menu.MainMenu


bombAmountFromDifficulty : Menu.GameDifficulty -> Int
bombAmountFromDifficulty difficulty =
    case difficulty of
        Menu.Easy ->
            9

        Menu.Medium ->
            40

        Menu.Hard ->
            99


bombsGenerator : Size -> Int -> Random.Generator (List Position)
bombsGenerator ( width, height ) amount =
    Random.list
        amount
        (Random.pair (Random.int 0 (width - 1)) (Random.int 0 (height - 1)))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = ChooseDifficulty
    | StartGame Menu.GameDifficulty
    | RestartGame
    | RevealTile Position
    | FlagTile Position
    | RevealNonFlaggedNeighbours Position
    | GeneratedBombs (List Position)
    | NoOp



-- VIEW


viewTileAt : GameState -> Position -> Maybe (Svg Msg)
viewTileAt { bombs, tiles, size } position =
    Dict.get position tiles
        |> Maybe.map
            (\tile ->
                viewTile tile position (tileBombCount bombs size)
                    |> Svg.map
                        (\m ->
                            case m of
                                Tile.RevealTile p ->
                                    RevealTile p

                                Tile.FlagTile p ->
                                    FlagTile p

                                Tile.RevealNonFlaggedNeighbours p ->
                                    RevealNonFlaggedNeighbours p

                                Tile.NoOp ->
                                    NoOp
                        )
            )


viewGameBoard : GameState -> Maybe String -> Svg Msg
viewGameBoard gameState opacity =
    svg
        [ SvgAttr.width (String.fromInt (Tile.screenWidth gameState.size))
        , SvgAttr.height (String.fromInt (Tile.screenHeight gameState.size))
        , SvgAttr.viewBox
            ("0 0 "
                ++ String.fromInt (Tile.screenWidth gameState.size)
                ++ " "
                ++ String.fromInt (Tile.screenHeight gameState.size)
            )
        , SvgAttr.opacity (Maybe.withDefault "1" opacity)
        ]
        (List.filterMap (viewTileAt gameState) (Map.positions gameState.size))


viewBlockedGameBoard : GameState -> String -> Html Msg
viewBlockedGameBoard gameState label =
    div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-items" "center"
        ]
        [ viewGameBoard gameState (Just "0.5")
        , span
            [ HtmlAttr.style "font-size" "20px"
            , HtmlAttr.style "fontFamily" "monospace"
            , HtmlAttr.style "margin-bottom" "10px"
            ]
            [ text label ]
        , button
            [ onClick RestartGame
            , HtmlAttr.style "font-size" "20px"
            , HtmlAttr.style "padding" "5px 15px"
            ]
            [ text "Back to Menu" ]
        ]


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-items" "center"
        ]
        [ h1
            [ HtmlAttr.style "fontFamily" "monospace"
            , HtmlAttr.style "font-size" "40px"
            ]
            [ text "Minesweeper" ]
        , case model of
            MainMenu menu ->
                Menu.view menu
                    |> Html.map
                        (\m ->
                            case m of
                                Menu.StartGame difficulty ->
                                    StartGame difficulty

                                Menu.ChooseDifficulty ->
                                    ChooseDifficulty
                        )

            Playing gameState ->
                viewGameBoard gameState Nothing

            Lost gameState ->
                viewBlockedGameBoard gameState "You lost"

            Won gameState ->
                viewBlockedGameBoard gameState "You won!"
        ]



-- UPDATE


updateGameTiles : GameState -> (Tile.Tile -> Tile.Tile) -> Position -> GameState
updateGameTiles game action position =
    { game | tiles = Dict.update position (Maybe.map action) game.tiles }


isEmptyWithNoNeighbourBombs : GameState -> Position -> Bool
isEmptyWithNoNeighbourBombs { size, bombs, tiles } position =
    (Dict.get position tiles == Just (Tile.Hidden Tile.Empty))
        && (Map.tileBombCount bombs size position == 0)


isRevealedWithAllFlags : GameState -> Position -> Bool
isRevealedWithAllFlags { size, bombs, tiles } position =
    let
        bombCount : Int
        bombCount =
            Map.tileBombCount bombs size position

        flagsCount : Int
        flagsCount =
            List.length
                (List.filter
                    (\p ->
                        case Dict.get p tiles of
                            Just (Tile.Flagged _) ->
                                True

                            _ ->
                                False
                    )
                    (Map.neighbours size position)
                )
    in
    (bombCount == flagsCount)
        && (Dict.get position tiles == Just (Tile.Revealed Tile.Empty))


revealNonFlaggedNeighbours : GameState -> Position -> GameState
revealNonFlaggedNeighbours game position =
    if isRevealedWithAllFlags game position then
        List.foldr (\x rec -> revealTileAndMaybeNeighbours rec x)
            game
            (List.filter
                (\p ->
                    case Dict.get p game.tiles of
                        Just (Tile.Hidden _) ->
                            True

                        _ ->
                            False
                )
                (Map.neighbours game.size position)
            )

    else
        game


revealedTiles : GameState -> Int
revealedTiles game =
    Dict.size
        (Dict.filter
            (\_ tile ->
                case tile of
                    Revealed Empty ->
                        True

                    _ ->
                        False
            )
            game.tiles
        )


gameToModel : GameState -> Model
gameToModel game =
    if
        List.any
            (\b ->
                case Dict.get b game.tiles of
                    Just (Revealed Bomb) ->
                        True

                    _ ->
                        False
            )
            game.bombs
    then
        Lost game

    else if
        (revealedTiles game + List.length game.bombs)
            == (Tuple.first game.size * Tuple.second game.size)
    then
        Won game

    else
        Playing game


revealTileAndMaybeNeighbours : GameState -> Position -> GameState
revealTileAndMaybeNeighbours game position =
    let
        newGame : GameState
        newGame =
            { game
                | tiles =
                    Dict.update position (Maybe.map Tile.reveal) game.tiles
            }
    in
    if isEmptyWithNoNeighbourBombs game position then
        List.foldr
            (\x rec -> revealTileAndMaybeNeighbours rec x)
            newGame
            (List.filter
                (\p ->
                    case Dict.get p game.tiles of
                        Just (Tile.Hidden _) ->
                            True

                        _ ->
                            False
                )
                (Map.neighbours game.size position)
            )

    else
        newGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( MainMenu Menu.MainMenu, ChooseDifficulty ) ->
            ( MainMenu Menu.ChoosingDifficulty, Cmd.none )

        ( MainMenu Menu.ChoosingDifficulty, StartGame difficulty ) ->
            let
                bombAmount : Int
                bombAmount =
                    bombAmountFromDifficulty difficulty

                size : Size
                size =
                    Map.sizeFromDifficulty difficulty
            in
            ( Playing { tiles = emptyTileMap size, bombs = [], size = size }
            , Random.generate GeneratedBombs (bombsGenerator size bombAmount)
            )

        ( Playing game, RevealTile p ) ->
            if List.member p game.bombs then
                ( Lost (revealTileAndMaybeNeighbours game p), Cmd.none )

            else
                ( Playing (revealTileAndMaybeNeighbours game p), Cmd.none )

        ( Playing game, RevealNonFlaggedNeighbours p ) ->
            ( gameToModel (revealNonFlaggedNeighbours game p), Cmd.none )

        ( Playing game, FlagTile p ) ->
            ( Playing (updateGameTiles game Tile.flag p), Cmd.none )

        ( Playing game, GeneratedBombs bombs ) ->
            ( Playing
                (let
                    nubBombs : List Position
                    nubBombs =
                        listNub bombs
                 in
                 { game
                    | bombs = nubBombs
                    , tiles =
                        List.foldr
                            (\bombPosition dict ->
                                Dict.update bombPosition
                                    (Maybe.map Tile.putBomb)
                                    dict
                            )
                            game.tiles
                            nubBombs
                 }
                )
            , Cmd.none
            )

        ( Lost _, RestartGame ) ->
            ( MainMenu Menu.MainMenu, Cmd.none )

        ( Won _, RestartGame ) ->
            ( MainMenu Menu.MainMenu, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
