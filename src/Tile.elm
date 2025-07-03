module Tile exposing
    ( Msg(..)
    , Tile(..)
    , TileContent(..)
    , TilePosition
    , defaultSize
    , flagTile
    , neighbours
    , putBomb
    , revealTile
    , screenSize
    , tileNumber
    , tileSize
    , viewTile
    )

import Json.Decode
import List exposing (member)
import String exposing (fromFloat)
import Svg exposing (Svg, g, rect, text, text_)
import Svg.Attributes as SvgAttr
import Svg.Events exposing (onClick, preventDefaultOn)


type alias ScreenPosition =
    ( Float, Float )


type TileContent
    = Bomb
    | Empty


type Tile
    = Revealed TileContent
    | Hidden TileContent
    | Flagged TileContent


type alias TilePosition =
    ( Int, Int )


type Msg
    = RevealTile TilePosition
    | FlagTile TilePosition


tileSize : number
tileSize =
    45


defaultSize : number
defaultSize =
    15


tileSpacing : number
tileSpacing =
    1


screenSize : number
screenSize =
    defaultSize * (tileSize + tileSpacing)


tileToScreenPosition : ( Int, Int ) -> ( Float, Float )
tileToScreenPosition ( x, y ) =
    ( toFloat x * (tileSize + tileSpacing)
    , toFloat y * (tileSize + tileSpacing)
    )


revealTile : Tile -> Tile
revealTile tile =
    case tile of
        Hidden c ->
            Revealed c

        Revealed _ ->
            tile

        Flagged _ ->
            tile


flagTile : Tile -> Tile
flagTile tile =
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


baseTile : ScreenPosition -> String -> Svg msg
baseTile ( x, y ) colorStr =
    let
        sTileSize =
            fromFloat tileSize
    in
    rect
        [ SvgAttr.x (fromFloat x)
        , SvgAttr.y (fromFloat y)
        , SvgAttr.width sTileSize
        , SvgAttr.height sTileSize
        , SvgAttr.fill colorStr
        ]
        []


tileText : ScreenPosition -> String -> Maybe String -> Svg msg
tileText ( x, y ) value color =
    text_
        [ SvgAttr.x (String.fromFloat (x + (tileSize / 2)))
        , SvgAttr.y (String.fromFloat (y + (tileSize / 2)))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.dominantBaseline "central"
        , SvgAttr.fontSize (String.fromFloat (tileSize * 0.75))
        , SvgAttr.fontFamily "monospace"
        , SvgAttr.fill (Maybe.withDefault "white" color)
        , SvgAttr.pointerEvents "none"
        ]
        [ text value ]


between : number -> number -> number -> Bool
between lo hi x =
    lo <= x && x < hi


neighbours : TilePosition -> List TilePosition
neighbours ( tileX, tileY ) =
    let
        validTileCoordinate =
            between 0 defaultSize
    in
    List.filter
        (\( x, y ) ->
            validTileCoordinate x
                && validTileCoordinate y
                && (( x, y ) /= ( tileX, tileY ))
        )
        (List.concatMap
            (\n -> List.map (\m -> ( tileX + n, tileY + m )) [ -1, 0, 1 ])
            [ -1, 0, 1 ]
        )


tileNumber : TilePosition -> List TilePosition -> Int
tileNumber tilePosition bombs =
    List.length (List.filter (\p -> member p bombs) (neighbours tilePosition))


tileBombCount : TilePosition -> ScreenPosition -> List TilePosition -> Svg msg
tileBombCount tilePosition screenPosition bombs =
    let
        n =
            tileNumber tilePosition bombs
    in
    tileText screenPosition (String.fromInt n) (neighbourBombsNumberColor n)


onRightClick : Msg -> Svg.Attribute Msg
onRightClick msg =
    preventDefaultOn "contextmenu" (Json.Decode.map (\msg1 -> ( msg1, True )) (Json.Decode.succeed msg))


viewTile : Tile -> List TilePosition -> TilePosition -> Svg Msg
viewTile tile bombs tilePosition =
    let
        screenPosition =
            tileToScreenPosition tilePosition
    in
    case tile of
        Revealed content ->
            case content of
                Bomb ->
                    g []
                        [ baseTile screenPosition "darkRed"
                        , tileText screenPosition "☠" (Just "white")
                        ]

                Empty ->
                    g []
                        [ baseTile screenPosition "lightGrey"
                        , tileBombCount tilePosition screenPosition bombs
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
