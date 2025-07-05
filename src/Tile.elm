module Tile exposing
    ( Msg(..)
    , Position
    , ScreenPosition
    , Tile
    , flag
    , hasBomb
    , hiddenEmpty
    , isEmpty
    , isFlagged
    , isHidden
    , isRevealed
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
    40


screenWidth : ( Int, Int ) -> Int
screenWidth ( width, _ ) =
    width * size


screenHeight : ( Int, Int ) -> Int
screenHeight ( _, height ) =
    height * size


tileToScreenPosition : ( Int, Int ) -> ( Float, Float )
tileToScreenPosition ( x, y ) =
    ( toFloat x * size, toFloat y * size )



-- CREATE


hiddenEmpty : Tile
hiddenEmpty =
    Hidden Empty



-- TRANSFORM


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



-- QUERY


isHidden : Tile -> Bool
isHidden tile =
    case tile of
        Hidden _ ->
            True

        _ ->
            False


isRevealed : Tile -> Bool
isRevealed tile =
    case tile of
        Revealed _ ->
            True

        _ ->
            False


isFlagged : Tile -> Bool
isFlagged tile =
    case tile of
        Flagged _ ->
            True

        _ ->
            False


isEmpty : Tile -> Bool
isEmpty tile =
    case tile of
        Revealed Empty ->
            True

        Hidden Empty ->
            True

        Flagged Empty ->
            True

        _ ->
            False


hasBomb : Tile -> Bool
hasBomb tile =
    case tile of
        Revealed Bomb ->
            True

        Hidden Bomb ->
            True

        Flagged Bomb ->
            True

        _ ->
            False


tileNeighbourBombs : Int -> ScreenPosition -> Svg Msg
tileNeighbourBombs n screenPosition =
    tileText screenPosition (String.fromInt n) (neighbourBombsNumberColor n)



-- SVG


baseTile : ScreenPosition -> String -> Svg msg
baseTile ( x, y ) colorStr =
    let
        sTileSize : String
        sTileSize =
            fromFloat size
    in
    rect
        [ SvgAttr.x (fromFloat x)
        , SvgAttr.y (fromFloat y)
        , SvgAttr.width sTileSize
        , SvgAttr.height sTileSize
        , SvgAttr.fill colorStr
        , SvgAttr.stroke "#848484"
        , SvgAttr.rx "3"
        , SvgAttr.ry "3"
        ]
        []


neighbourBombsNumberColor : Int -> Maybe String
neighbourBombsNumberColor n =
    case n of
        0 ->
            Just "lightGrey"

        1 ->
            Just "#0000ff"

        2 ->
            Just "#007b00"

        3 ->
            Just "#ff0000"

        4 ->
            Just "#00007b"

        5 ->
            Just "#7b0000"

        6 ->
            Just "#008080"

        7 ->
            Just "#000000"

        8 ->
            Just "#808080"

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


onRightClick : Msg -> Svg.Attribute Msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Json.Decode.succeed ( msg, True ))


viewTile : Tile -> Position -> Int -> Svg Msg
viewTile tile tilePosition tileBombCount =
    let
        screenPosition : ScreenPosition
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
                        , tileNeighbourBombs tileBombCount screenPosition
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
                [ baseTile screenPosition "#bdbdbd" ]
