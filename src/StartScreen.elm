module StartScreen exposing
    ( Model(..)
    , Msg(..)
    , view
    )

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Msg
    = StartGame


type Model
    = Waiting


view : Model -> Html Msg
view _ =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ button
            [ onClick StartGame
            , style "font-size" "20px"
            , style "padding" "5px 15px"
            ]
            [ text "Jugar" ]
        ]
