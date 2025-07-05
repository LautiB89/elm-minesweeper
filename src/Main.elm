module Main exposing
    ( GameState
    , Model(..)
    , Msg(..)
    , main
    )

import Browser
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Menu
import Random
import Random.List as RandomList
import Svg exposing (Svg, svg)
import Svg.Attributes as SvgAttr
import Tile exposing (Position, reveal)
import TileMap exposing (Size, TileMap, anyIsRevealed, totalTiles)


type alias GameState =
    { tileMap : TileMap, bombs : List Position }


type Model
    = MainMenu Menu.Model
    | FirstMove Menu.GameDifficulty
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


bombsGenerator : Position -> Size -> Int -> Random.Generator (List Position)
bombsGenerator initialPosition size amount =
    let
        positions : List Position
        positions =
            List.filter (\p -> p /= initialPosition) (TileMap.positions size)
    in
    RandomList.choices amount positions
        |> Random.map Tuple.first


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
    | ReGenerateBombsAndReveal Position (List Position)
    | NoOp



-- VIEW


viewTileMap : TileMap -> Maybe String -> Svg Msg
viewTileMap tileMap opacity =
    svg
        [ SvgAttr.width (String.fromInt (Tile.screenWidth tileMap.size))
        , SvgAttr.height (String.fromInt (Tile.screenHeight tileMap.size))
        , SvgAttr.viewBox
            ("0 0 "
                ++ String.fromInt (Tile.screenWidth tileMap.size)
                ++ " "
                ++ String.fromInt (Tile.screenHeight tileMap.size)
            )
        , SvgAttr.opacity (Maybe.withDefault "1" opacity)
        ]
        (List.filterMap (\p -> viewTileAt p tileMap) (TileMap.positions tileMap.size))


viewTileAt : Position -> TileMap -> Maybe (Svg Msg)
viewTileAt position tileMap =
    TileMap.get position tileMap
        |> Maybe.map
            (\tile ->
                Tile.viewTile tile position (TileMap.countBombNeighbours tileMap position)
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


viewBlockedGameBoard : GameState -> String -> Html Msg
viewBlockedGameBoard gameState label =
    div
        [ HtmlAttr.style "display" "flex"
        , HtmlAttr.style "flex-direction" "column"
        , HtmlAttr.style "align-items" "center"
        ]
        [ viewTileMap gameState.tileMap (Just "0.5")
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

            FirstMove difficulty ->
                viewTileMap (TileMap.empty difficulty) Nothing

            Playing gameState ->
                viewTileMap gameState.tileMap Nothing

            Lost gameState ->
                viewBlockedGameBoard
                    { gameState
                        | tileMap =
                            List.foldr
                                (\p rec -> TileMap.update p (\m -> Maybe.map reveal m) rec)
                                gameState.tileMap
                                gameState.bombs
                    }
                    "You lost"

            Won gameState ->
                viewBlockedGameBoard gameState "You won!"
        ]



-- UPDATE


tileMapToModel : TileMap -> List Position -> Model
tileMapToModel tileMap bombs =
    let
        game : GameState
        game =
            { tileMap = tileMap, bombs = bombs }
    in
    if anyIsRevealed game.bombs tileMap then
        Lost game

    else if (TileMap.revealedTiles tileMap + List.length bombs) == totalTiles tileMap then
        Won game

    else
        Playing game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( MainMenu Menu.MainMenu, ChooseDifficulty ) ->
            ( MainMenu Menu.ChoosingDifficulty, Cmd.none )

        ( MainMenu Menu.ChoosingDifficulty, StartGame difficulty ) ->
            ( FirstMove difficulty, Cmd.none )

        ( FirstMove difficulty, RevealTile p ) ->
            ( FirstMove difficulty
            , Random.generate
                (ReGenerateBombsAndReveal p)
                (bombsGenerator p
                    (TileMap.sizeFromDifficulty difficulty)
                    (bombAmountFromDifficulty difficulty)
                )
            )

        ( FirstMove difficulty, ReGenerateBombsAndReveal p bombs ) ->
            ( let
                game : GameState
                game =
                    { bombs = bombs
                    , tileMap =
                        List.foldr
                            (\bombPosition rec ->
                                TileMap.update bombPosition
                                    (Maybe.map Tile.putBomb)
                                    rec
                            )
                            (TileMap.empty difficulty)
                            bombs
                    }
              in
              tileMapToModel
                (TileMap.revealTileAndMaybeNeighbours game.tileMap p)
                game.bombs
            , Cmd.none
            )

        ( Playing game, RevealTile p ) ->
            ( tileMapToModel
                (TileMap.revealTileAndMaybeNeighbours game.tileMap p)
                game.bombs
            , Cmd.none
            )

        ( Playing game, RevealNonFlaggedNeighbours p ) ->
            ( tileMapToModel
                (TileMap.revealNonFlaggedNeighbours game.tileMap p)
                game.bombs
            , Cmd.none
            )

        ( Playing game, FlagTile p ) ->
            ( Playing
                { game | tileMap = TileMap.update p (Maybe.map Tile.flag) game.tileMap }
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
