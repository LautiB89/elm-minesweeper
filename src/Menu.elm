module Menu exposing
    ( GameDifficulty(..)
    , Model(..)
    , Msg(..)
    , view
    )

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type GameDifficulty
    = Easy
    | Medium
    | Hard


type Msg
    = StartGame GameDifficulty
    | ChooseDifficulty


type Model
    = MainMenu
    | ChoosingDifficulty


baseButton : String -> Msg -> Html Msg
baseButton label msg =
    button
        [ onClick msg
        , style "font-size" "30px"
        , style "padding" "5px 15px"
        ]
        [ text label ]


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "gap" "10px"
        , style "align-items" "center"
        ]
        (case model of
            MainMenu ->
                [ baseButton "Start Game" ChooseDifficulty ]

            ChoosingDifficulty ->
                [ baseButton "Easy" (StartGame Easy)
                , baseButton "Medium" (StartGame Medium)
                , baseButton "Hard" (StartGame Hard)
                ]
        )
