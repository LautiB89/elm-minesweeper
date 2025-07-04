module Tile exposing
    ( Content(..)
    , Msg(..)
    , Position
    , ScreenPosition
    , Tile(..)
    , flag
    , putBomb
    , reveal
    , screenHeight
    , screenWidth
    , viewTile
    )

import Json.Decode
import String exposing (fromFloat)
import Svg exposing (Svg, g, rect, text, text_)
import Svg.Attributes as SvgAttr
import Svg.Events exposing (onClick, preventDefaultOn)


type alias ScreenPosition =
    ( Float, Float )


type Content
    = Bomb
    | Empty


type Tile
    = Revealed Content
    | Hidden Content
    | Flagged Content


type alias Position =
    ( Int, Int )


type Msg
    = RevealTile Position
    | RevealNonFlaggedNeighbours Position
    | FlagTile Position
    | NoOp


size : number
size =
    30


spacing : number
spacing =
    1


screenWidth : ( Int, Int ) -> Int
screenWidth ( width, _ ) =
    width * (size + spacing)


screenHeight : ( Int, Int ) -> Int
screenHeight ( _, height ) =
    height * (size + spacing)


tileToScreenPosition : ( Int, Int ) -> ( Float, Float )
tileToScreenPosition ( x, y ) =
    ( toFloat x * (size + spacing), toFloat y * (size + spacing) )


reveal : Tile -> Tile
reveal tile =
    case tile of
        Hidden c ->
            Revealed c

        Revealed _ ->
            tile

        Flagged _ ->
            tile


flag : Tile -> Tile
flag tile =
    case tile of
        Revealed _ ->
            tile

        Hidden c ->
            Flagged c

        Flagged c ->
            Hidden c


putBomb : Tile -> Tile
putBomb tile =
    case tile of
        Revealed _ ->
            Revealed Bomb

        Hidden _ ->
            Hidden Bomb

        Flagged _ ->
            Flagged Bomb


baseTile : ScreenPosition -> String -> Svg msg
baseTile ( x, y ) colorStr =
    let
        sTileSize =
            fromFloat size
    in
    rect
        [ SvgAttr.x (fromFloat x)
        , SvgAttr.y (fromFloat y)
        , SvgAttr.width sTileSize
        , SvgAttr.height sTileSize
        , SvgAttr.fill colorStr
        ]
        []


neighbourBombsNumberColor : Int -> Maybe String
neighbourBombsNumberColor n =
    case n of
        0 ->
            Just "lightGrey"

        1 ->
            Just "blue"

        2 ->
            Just "green"

        3 ->
            Just "red"

        4 ->
            Just "darkBlue"

        5 ->
            Just "brown"

        6 ->
            Just "cyan"

        7 ->
            Just "black"

        8 ->
            Just "lightGrey"

        _ ->
            Nothing


tileText : ScreenPosition -> String -> Maybe String -> Svg Msg
tileText ( x, y ) value color =
    text_
        [ SvgAttr.x (String.fromFloat (x + (size / 2)))
        , SvgAttr.y (String.fromFloat (y + (size / 2)))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "central"
        , SvgAttr.fontSize (String.fromFloat (size * 0.75))
        , SvgAttr.fontFamily "monospace"
        , SvgAttr.fill (Maybe.withDefault "white" color)
        , SvgAttr.pointerEvents "none"
        ]
        [ text value ]


tileNeighbourBombs : Int -> ScreenPosition -> Svg Msg
tileNeighbourBombs n screenPosition =
    tileText screenPosition (String.fromInt n) (neighbourBombsNumberColor n)


onRightClick : Msg -> Svg.Attribute Msg
onRightClick msg =
    preventDefaultOn
        "contextmenu"
        (Json.Decode.succeed ((\msg1 -> ( msg1, True )) msg))


viewTile : Tile -> Position -> (Position -> Int) -> Svg Msg
viewTile tile tilePosition tileBombCount =
    let
        screenPosition =
            tileToScreenPosition tilePosition
    in
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    g [ onRightClick NoOp ]
                        [ baseTile screenPosition "darkRed"
                        , tileText screenPosition "☠" (Just "white")
                        ]

                Empty ->
                    g
                        [ onClick (RevealNonFlaggedNeighbours tilePosition)
                        , onRightClick NoOp
                        ]
                        [ baseTile screenPosition "lightGrey"
                        , tileNeighbourBombs (tileBombCount tilePosition) screenPosition
                        ]

        Flagged _ ->
            g [ onRightClick (FlagTile tilePosition) ]
                [ baseTile screenPosition "grey"
                , tileText screenPosition "🏳" (Just "white")
                ]

        Hidden _ ->
            g
                [ onRightClick (FlagTile tilePosition)
                , onClick (RevealTile tilePosition)
                ]
                [ baseTile screenPosition "darkGrey" ]
