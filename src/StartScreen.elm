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
        [ style "width" "420px", style "max-width" "100%" ]
        [ button [ onClick StartGame ] [ text "Jugar" ] ]
