port module Main exposing (..)

import Array
import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eval
import Lang
import MiniRte as Rte
import MiniRte.Types as RteTypes
import Parser exposing (Problem(..), int)
import Set
import Zadaci


port fromBrowserClipboard : (String -> msg) -> Sub msg


port toBrowserClipboard : String -> Cmd msg


type Page
    = Home
    | Zadatak Int
    | Summary


type Msg
    = Editor RteTypes.Msg
    | ChangeInitialRegs Int
    | ChangeRegister Int Int
    | Freeze
    | UnFreeze
    | ChangePage Page
    | TestProgram
    | EvalProgram
    | NoOp


type alias Model =
    { rte : Rte.Rte
    , initialRegs : Eval.Registers
    , program : Maybe Lang.Program
    , errors : Maybe (List Parser.DeadEnd)
    , result : Maybe Eval.Registers
    , page : Page
    , solved : Set.Set Int
    }


main : Program () Model Msg
main =
    Browser.document
        { init =
            \() ->
                ( { rte = Rte.init "editor"
                  , initialRegs = Array.fromList [ 1, 2 ]
                  , page = Home
                  , program = Nothing
                  , errors = Nothing
                  , solved = Set.empty
                  , result = Nothing
                  }
                , Cmd.none
                )
        , update = update
        , view =
            \model ->
                { title = "Register Machine"
                , body =
                    view model
                        |> Element.layout [ Element.padding 60 ]
                        |> List.singleton
                }
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map Editor <| Rte.subscriptions model.rte
                    , Sub.map Editor <| fromBrowserClipboard RteTypes.FromBrowserClipboard
                    ]
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Editor (RteTypes.ToBrowserClipboard txt) ->
            ( model, toBrowserClipboard txt )

        Editor rteMsg ->
            let
                ( m, cmd ) =
                    Rte.update rteMsg model.rte

                ( m2, _ ) =
                    update EvalProgram { model | rte = m }
            in
            ( m2, Cmd.map Editor cmd )

        ChangeInitialRegs n ->
            ( { model
                | initialRegs = Eval.extendRegisters model.initialRegs n
              }
            , Cmd.none
            )

        ChangeRegister i x ->
            ( { model
                | initialRegs =
                    Eval.setRegister i x model.initialRegs
              }
            , Cmd.none
            )

        Freeze ->
            ( { model | rte = model.rte |> Rte.freeze }, Cmd.none )

        UnFreeze ->
            ( { model | rte = model.rte |> Rte.unfreeze }, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )

        TestProgram ->
            ( { model
                | solved =
                    case ( model.page, model.program ) of
                        ( Zadatak n, Just program ) ->
                            if Zadaci.passAll n program then
                                Set.insert n model.solved

                            else
                                model.solved

                        _ ->
                            model.solved
              }
            , Cmd.none
            )

        EvalProgram ->
            let
                res =
                    Rte.content model.rte |> Rte.contentToText |> String.dropRight 1 |> Lang.parse
            in
            ( { model
                | program = Result.toMaybe res
                , result = Maybe.map (Eval.eval model.initialRegs) <| Result.toMaybe res
                , errors =
                    case res of
                        Err er ->
                            Just er

                        _ ->
                            Nothing
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


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


view : Model -> Element.Element Msg
view model =
    let
        text =
            Rte.content model.rte |> Rte.contentToText |> String.dropRight 1

        lineNums =
            text |> String.lines |> List.length |> List.range 1

        buttonStyle =
            [ Element.padding 10
            , Element.Background.color <| Element.rgb255 100 100 100
            , Element.width <| Element.px 50
            ]

        viewZadNum ( i, _ ) =
            Element.Input.button
                (buttonStyle
                    ++ (if Set.member i model.solved then
                            [ Element.Background.color <| Element.rgb255 0 255 0 ]

                        else
                            []
                       )
                )
                { onPress = Just <| ChangePage <| Zadatak i
                , label = Element.text <| String.fromInt (i + 1)
                }

        viewTextArea =
            Rte.textarea model.rte []
                |> Element.html
                |> Element.map Editor
                |> Element.el
                    [ Element.Border.solid
                    , Element.Border.width 1
                    , Element.width <| Element.px 200
                    , Element.alignTop
                    , Element.Events.onMouseEnter <| UnFreeze
                    ]

        viewEditor =
            Element.row [ Element.Events.onFocus UnFreeze ]
                [ lineNums
                    |> List.map (Element.text << String.fromInt)
                    |> Element.column
                        [ Element.paddingXY 5 0
                        ]
                , viewTextArea
                ]

        viewZad =
            case model.page of
                Home ->
                    Element.text "Editor"
                        |> Element.el [ Element.padding 10 ]

                Zadatak n ->
                    case Array.get n Zadaci.zadaci of
                        Nothing ->
                            Element.none

                        Just zad ->
                            [ Element.text zad.text ]
                                |> Element.paragraph [ Element.padding 10 ]

                Summary ->
                    Element.text "Summary"

        viewInitial =
            Element.column infoBox
                [ Element.Input.slider
                    [ Element.height (Element.px 30)
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Element.Background.color <| Element.rgb 0.5 0.5 0.5
                            , Element.Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { label = Element.Input.labelAbove [] <| Element.text "Num of registers"
                    , onChange = ChangeInitialRegs << floor
                    , min = 0
                    , max = 10
                    , value = model.initialRegs |> Array.length |> toFloat
                    , step = Just 1
                    , thumb = Element.Input.defaultThumb
                    }
                , model.initialRegs
                    |> Eval.toIndexedRegisterList
                    |> List.map viewAssigmentChangable
                    |> Element.column []
                ]

        viewAssigment ( var, val ) =
            String.fromInt var ++ " = " ++ String.fromInt val |> Element.text

        viewAssigmentChangable ( var, val ) =
            Element.row []
                [ String.fromInt var ++ " = " |> Element.text
                , Element.Input.text [ Element.Events.onFocus Freeze ]
                    { onChange =
                        \s ->
                            case Parser.run int s of
                                Ok newVal ->
                                    ChangeRegister var newVal

                                _ ->
                                    NoOp
                    , text = String.fromInt val
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "Value of Register"
                    }
                ]

        viewState =
            Element.column infoBox
                [ Element.text "⚡State: "
                , model.result
                    |> Maybe.withDefault Array.empty
                    |> Eval.toIndexedRegisterList
                    |> List.map viewAssigment
                    |> Element.column []
                ]
    in
    Element.row [ Element.width Element.fill, Element.centerX ]
        [ Element.column [ Element.spacing 50, Element.alignTop, Element.height Element.fill ]
            [ Element.Input.button buttonStyle
                { onPress = Just <| ChangePage Home
                , label = Element.text "📓"
                }
            , Element.column [ Element.spacing 10 ] <|
                List.map viewZadNum <|
                    Array.toIndexedList Zadaci.zadaci
            , Element.Input.button buttonStyle
                { onPress = Just <| ChangePage Summary
                , label = Element.text "Σ"
                }
            ]
        , Element.el [ Element.width Element.fill, Element.alignTop ] <|
            Element.column
                [ Element.spacing 10
                , Element.padding 10
                , Element.centerX
                ]
                [ viewZad, viewEditor, model.errors |> viewParsingErrors ]
        , Element.column [ Element.alignTop ]
            [ viewInitial, viewState ]
        ]


infoBox : List (Element.Attr () msg)
infoBox =
    [ Element.Background.color <| Element.rgb255 240 240 240
    , Element.spacing 10
    , Element.padding 10
    , Element.height Element.fill
    , Element.width Element.fill
    ]


viewParsingErrors : Maybe (List Parser.DeadEnd) -> Element.Element Msg
viewParsingErrors merrors =
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
    case merrors of
        Just errors ->
            Element.column infoBox
                [ Element.row [ Element.spacing 10 ]
                    [ "⚠️ Error parsing "
                        ++ (errors |> List.length |> String.fromInt)
                        ++ " posibilities"
                        |> Element.text
                        |> Element.el []
                    , Element.Input.button
                        [ Element.Background.color <| Element.rgb 0.5 0.5 0.5
                        , Element.padding 10
                        ]
                        { onPress = Just TestProgram
                        , label = Element.text "TEST"
                        }
                    ]
                , errors |> List.map viewParsingError |> Element.column []
                ]

        Nothing ->
            Element.row [ Element.spacing 10 ]
                [ "⚠️ No errors "
                    |> Element.text
                    |> Element.el []
                , Element.Input.button
                    [ Element.Background.color <| Element.rgb 0.5 0.5 0.5
                    , Element.padding 10
                    ]
                    { onPress = Just TestProgram
                    , label = Element.text "TEST"
                    }
                ]
