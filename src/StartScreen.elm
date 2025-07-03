module StartScreen exposing
    ( Model(..)
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Msg
    = StartGame


type Model
    = Waiting


init : Model
init =
    Waiting


update : a -> b -> c -> c
update events msg model =
    model


view : Model -> Html Msg
view model =
    div
        [ style "width" "420px", style "max-width" "100%" ]
        [ button [ onClick StartGame ] [ text "Jugar" ] ]
