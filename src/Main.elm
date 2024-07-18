port module Main exposing (..)

import Array exposing (Array)
import Browser
import Element
import Element.Background
import Element.Border
import Element.Font
import Eval
import Html.Attributes as HA
import Lang
import MiniRte as Editor
import MiniRte.Types as RteTypes exposing (Msg(..))
import Parser exposing (Problem(..))


port fromBrowserClipboard : (String -> msg) -> Sub msg


port toBrowserClipboard : String -> Cmd msg


main : Program () Editor.Rte Msg
main =
    Browser.document
        { init =
            \() -> ( Editor.init "editor", Cmd.none )
        , update =
            \msg model ->
                case msg of
                    RteTypes.ToBrowserClipboard txt ->
                        ( model, toBrowserClipboard txt )

                    rteMsg ->
                        Editor.update rteMsg model
        , view =
            \model ->
                { title = "Register Machine"
                , body =
                    view model
                        |> Element.layout [ Element.padding 100 ]
                        |> List.singleton
                }
        , subscriptions =
            \model ->
                Sub.batch
                    [ Editor.subscriptions model
                    , fromBrowserClipboard FromBrowserClipboard
                    ]
        }


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        ExpectingSymbol symbol ->
            "Expecting `" ++ symbol ++ "`"

        ExpectingEnd ->
            "Expecting End. Did you forget `newline`?"

        ExpectingInt ->
            "Expecting int like `13`"

        Problem msg ->
            msg

        err ->
            "Unknown error: " ++ Debug.toString err


errorOr : err -> Result err b -> err
errorOr err res =
    case res of
        Result.Ok _ ->
            err

        Result.Err x ->
            x


unwrapResult : Result a a -> a
unwrapResult res =
    case res of
        Result.Ok x ->
            x

        Result.Err x ->
            x


view : Editor.Rte -> Element.Element Msg
view model =
    let
        res =
            Editor.content model
                |> Editor.contentToText
                |> String.trim
                |> Lang.parse
                |> Result.map Eval.eval
    in
    Element.column [ Element.width Element.fill ]
        [ Element.row
            [ Element.Border.solid
            , Element.Border.width 1
            , Element.width Element.fill
            ]
            [ Element.html <|
                Editor.textarea model []
            ]
        , Element.row [ Element.centerX ]
            [ res
                |> Result.withDefault Array.empty
                |> viewState
            , res
                |> errorOr []
                |> viewParsingErrors
            ]
        ]


viewState : Array Int -> Element.Element msg
viewState state =
    Element.column
        [ Element.pointer
        , Element.htmlAttribute <| HA.style "box-shadow" "3px 3px 4px black"
        , Element.Background.color <| Element.rgb255 240 240 240
        , Element.spacing 10
        , Element.padding 10
        , Element.height <| Element.fill
        , Element.width <| Element.fill
        ]
        [ Element.text
            "⚡State: "
            |> Element.el
                [ Element.Border.width 0

                -- , Element.Background.color <| Element.rgb 192 192 192
                , Element.width <| Element.px <| 6 * 18
                ]
        , state
            |> Array.toIndexedList
            |> List.map
                (\( var, val ) ->
                    String.fromInt var
                        ++ " = "
                        ++ String.fromInt val
                        |> Element.text
                        |> Element.el []
                )
            |> Element.column []
        ]


viewParsingErrors : List Parser.DeadEnd -> Element.Element msg
viewParsingErrors errors =
    let
        viewParsingError { col, problem, row } =
            Element.row []
                [ problemToString problem |> Element.text |> Element.el []
                , "["
                    ++ String.fromInt col
                    ++ ","
                    ++ String.fromInt row
                    ++ "]"
                    |> Element.text
                    |> Element.el
                        [ Element.Font.color <| Element.rgb255 192 192 192
                        ]
                ]
    in
    Element.column
        [ Element.pointer
        , Element.htmlAttribute <| HA.style "box-shadow" "3px 3px 4px black"
        , Element.Background.color <| Element.rgb255 240 240 240
        , Element.spacing 10
        , Element.padding 10
        , Element.height <| Element.fill
        , Element.width <| Element.fill
        ]
        [ "⚠️ Error parsing "
            ++ (errors |> List.length |> String.fromInt)
            ++ " posibilities"
            |> Element.text
            |> Element.el []
        , errors
            |> List.map viewParsingError
            |> Element.column []
        ]
