module Editor exposing (..)

import Array exposing (Array)
import Element
import Element.Background
import Element.Events
import Element.Font
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing (Decoder)


fontSize : Float
fontSize =
    20


lineHeight : Float
lineHeight =
    fontSize * 1.2


getText : { a | lines : Array String } -> String
getText { lines } =
    Array.toList lines
        |> String.join "\n"
        |> String.replace "\t" " "


type alias Model =
    { lines : Array String
    , cursor : Position
    , hover : Hover
    , selection : Selection
    }


type Hover
    = NoHover
    | HoverLine Int
    | HoverChar Position


type Selection
    = NoSelection
    | SelectingFrom Hover
    | SelectedChar Position
    | Selection Position Position


type alias Position =
    { line : Int
    , column : Int
    }


type Msg
    = NoOp
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | NewLine
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | Hover Hover
    | GoToHoveredPosition
    | StartSelecting
    | StopSelecting
    | Tab


init : Model
init =
    { lines = Array.fromList [ "" ]
    , cursor = Position 0 0
    , hover = NoHover
    , selection = NoSelection
    }


keyToMsg : ( Bool, String ) -> Decoder Msg
keyToMsg ( ctrl, string ) =
    case ( String.uncons string, ctrl ) of
        -- ( Just ( 'v', "" ), True ) ->
        --     JD.succeed Paste
        ( Just ( char, "" ), False ) ->
            JD.succeed (InsertChar char)

        _ ->
            case string of
                "Tab" ->
                    JD.succeed Tab

                "ArrowUp" ->
                    JD.succeed MoveUp

                "ArrowDown" ->
                    JD.succeed MoveDown

                "ArrowLeft" ->
                    JD.succeed MoveLeft

                "ArrowRight" ->
                    JD.succeed MoveRight

                "Backspace" ->
                    JD.succeed RemoveCharBefore

                "Delete" ->
                    JD.succeed RemoveCharAfter

                "Enter" ->
                    JD.succeed NewLine

                _ ->
                    JD.fail "This key does nothing"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MoveUp ->
            ( { model | cursor = moveUp model.cursor model.lines }
            , Cmd.none
            )

        MoveDown ->
            ( { model | cursor = moveDown model.cursor model.lines }
            , Cmd.none
            )

        MoveLeft ->
            ( { model | cursor = moveLeft model.cursor model.lines }
            , Cmd.none
            )

        MoveRight ->
            ( { model | cursor = moveRight model.cursor model.lines }
            , Cmd.none
            )

        NewLine ->
            ( newLine model
                |> sanitizeHover
            , Cmd.none
            )

        Tab ->
            -- ( String.foldl (\c -> \m -> insertChar c m) model "    "
            -- , Cmd.none
            -- )
            ( insertChar '\t' model
            , Cmd.none
            )

        InsertChar char ->
            ( insertChar char model
            , Cmd.none
            )

        RemoveCharBefore ->
            ( removeCharBefore model
                |> sanitizeHover
            , Cmd.none
            )

        RemoveCharAfter ->
            ( removeCharAfter model
                |> sanitizeHover
            , Cmd.none
            )

        Hover hover ->
            ( { model | hover = hover }
                |> sanitizeHover
            , Cmd.none
            )

        GoToHoveredPosition ->
            ( { model
                | cursor =
                    case model.hover of
                        NoHover ->
                            model.cursor

                        HoverLine line ->
                            { line = line
                            , column = lastColumn model.lines line
                            }

                        HoverChar position ->
                            position
              }
            , Cmd.none
            )

        StartSelecting ->
            ( { model | selection = SelectingFrom model.hover }
            , Cmd.none
            )

        StopSelecting ->
            -- Selection for all other
            let
                endHover =
                    model.hover

                newSelection =
                    case model.selection of
                        NoSelection ->
                            NoSelection

                        SelectingFrom startHover ->
                            if startHover == endHover then
                                case startHover of
                                    NoHover ->
                                        NoSelection

                                    HoverLine _ ->
                                        NoSelection

                                    HoverChar position ->
                                        SelectedChar position

                            else
                                hoversToPositions model.lines startHover endHover
                                    |> Maybe.map (\( from, to ) -> Selection from to)
                                    |> Maybe.withDefault NoSelection

                        SelectedChar _ ->
                            NoSelection

                        Selection _ _ ->
                            NoSelection
            in
            ( { model | selection = newSelection }
            , Cmd.none
            )


hoversToPositions : Array String -> Hover -> Hover -> Maybe ( Position, Position )
hoversToPositions lines from to =
    let
        selectionLinePosition : Int -> Position -> ( Position, Position )
        selectionLinePosition line position =
            if line >= position.line then
                ( position
                , { line = line, column = lastColumn lines line }
                )

            else
                ( { line = line + 1, column = 0 }
                , position
                )
    in
    case ( from, to ) of
        ( NoHover, _ ) ->
            Nothing

        ( _, NoHover ) ->
            Nothing

        ( HoverLine line1, HoverLine line2 ) ->
            let
                smaller =
                    min line1 line2

                bigger =
                    max line1 line2
            in
            Just
                ( { line = smaller + 1, column = 0 }
                , { line = bigger, column = lastColumn lines bigger }
                )

        ( HoverLine line, HoverChar position ) ->
            Just (selectionLinePosition line position)

        ( HoverChar position, HoverLine line ) ->
            Just (selectionLinePosition line position)

        ( HoverChar position1, HoverChar position2 ) ->
            let
                ( smaller, bigger ) =
                    if positionsLess position1 position2 then
                        ( position1, position2 )

                    else
                        ( position2, position1 )
            in
            Just ( smaller, bigger )


positionsLess : Position -> Position -> Bool
positionsLess from to =
    from.line < to.line || (from.line == to.line && from.column < to.column)


sanitizeHover : Model -> Model
sanitizeHover model =
    { model
        | hover =
            case model.hover of
                NoHover ->
                    model.hover

                HoverLine line ->
                    HoverLine (clamp 0 (lastLine model.lines) line)

                HoverChar { line, column } ->
                    let
                        sanitizedLine =
                            clamp 0 (lastLine model.lines) line

                        sanitizedColumn =
                            clamp 0 (lastColumn model.lines sanitizedLine) column
                    in
                    HoverChar
                        { line = sanitizedLine
                        , column = sanitizedColumn
                        }
    }


newLine : Model -> Model
newLine ({ cursor, lines } as model) =
    let
        { line, column } =
            cursor

        linesList : List String
        linesList =
            Array.toList lines

        line_ : Int
        line_ =
            line + 1

        contentUntilCursor : List String
        contentUntilCursor =
            linesList
                |> List.take line_
                |> List.indexedMap
                    (\i content ->
                        if i == line then
                            String.left column content

                        else
                            content
                    )

        restOfLineAfterCursor : String
        restOfLineAfterCursor =
            String.dropLeft column (lineContent lines line)

        restOfLines : List String
        restOfLines =
            List.drop line_ linesList

        newLines : Array String
        newLines =
            (contentUntilCursor
                ++ restOfLineAfterCursor
                :: restOfLines
            )
                |> Array.fromList

        newCursor : Position
        newCursor =
            { line = line_
            , column = 0
            }
    in
    { model
        | lines = newLines
        , cursor = newCursor
    }


insertChar : Char -> Model -> Model
insertChar char ({ cursor, lines } as model) =
    let
        { line, column } =
            cursor

        lineWithCharAdded : String -> String
        lineWithCharAdded content =
            String.left column content
                ++ String.fromChar char
                ++ String.dropLeft column content

        newLines : Array String
        newLines =
            lines
                |> Array.indexedMap
                    (\i content ->
                        if i == line then
                            lineWithCharAdded content

                        else
                            content
                    )

        newCursor : Position
        newCursor =
            { line = line
            , column = column + 1
            }
    in
    { model
        | lines = newLines
        , cursor = newCursor
    }


removeCharBefore : Model -> Model
removeCharBefore ({ cursor, lines } as model) =
    if cursor.line == 0 && cursor.column == 0 then
        model

    else
        let
            { line, column } =
                cursor

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line - 1 then
                    if column == 0 then
                        [ content ++ lineContent lines line ]

                    else
                        [ content ]

                else if lineNum == line then
                    if column == 0 then
                        []

                    else
                        [ String.left (column - 1) content
                            ++ String.dropLeft column content
                        ]

                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { model
            | lines = newLines
            , cursor = moveLeft cursor lines
        }


removeCharAfter : Model -> Model
removeCharAfter ({ cursor, lines } as model) =
    if isEndOfDocument lines cursor then
        model

    else
        let
            { line, column } =
                cursor

            isOnLastColumn : Bool
            isOnLastColumn =
                isLastColumn lines line column

            removeCharFromLine : ( Int, String ) -> List String
            removeCharFromLine ( lineNum, content ) =
                if lineNum == line then
                    if isOnLastColumn then
                        [ content ++ lineContent lines (line + 1) ]

                    else
                        [ String.left column content
                            ++ String.dropLeft (column + 1) content
                        ]

                else if lineNum == line + 1 then
                    if isOnLastColumn then
                        []

                    else
                        [ content ]

                else
                    [ content ]

            newLines : Array String
            newLines =
                lines
                    |> Array.toIndexedList
                    |> List.concatMap removeCharFromLine
                    |> Array.fromList
        in
        { model
            | lines = newLines
            , cursor = cursor
        }


moveUp : Position -> Array String -> Position
moveUp { line, column } lines =
    if line == 0 then
        { line = 0
        , column = 0
        }

    else
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveDown : Position -> Array String -> Position
moveDown { line, column } lines =
    if line == lastLine lines then
        { line = lastLine lines
        , column = lastColumn lines (lastLine lines)
        }

    else
        let
            line_ : Int
            line_ =
                nextLine lines line
        in
        { line = line_
        , column = clampColumn lines line_ column
        }


moveLeft : Position -> Array String -> Position
moveLeft ({ line, column } as position) lines =
    if line == 0 && column == 0 then
        position

    else if column == 0 then
        let
            line_ : Int
            line_ =
                previousLine line
        in
        { line = line_
        , column = lastColumn lines line_
        }

    else
        { line = line
        , column = column - 1
        }


moveRight : Position -> Array String -> Position
moveRight ({ line, column } as position) lines =
    if isEndOfDocument lines position then
        position

    else if isLastColumn lines line column then
        { line = nextLine lines line
        , column = 0
        }

    else
        { line = line
        , column = column + 1
        }


isEndOfDocument : Array String -> Position -> Bool
isEndOfDocument lines { line, column } =
    line
        == lastLine lines
        && isLastColumn lines line column


isLastColumn : Array String -> Int -> Int -> Bool
isLastColumn lines line column =
    column == lastColumn lines line


lastLine : Array String -> Int
lastLine lines =
    Array.length lines - 1


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : Array String -> Int -> Int
nextLine lines line =
    (line + 1)
        |> min (lastLine lines)


lastColumn : Array String -> Int -> Int
lastColumn lines line =
    lineLength lines line


clampColumn : Array String -> Int -> Int -> Int
clampColumn lines line column =
    column
        |> clamp 0 (lineLength lines line)


lineContent : Array String -> Int -> String
lineContent lines lineNum =
    lines
        |> Array.get lineNum
        |> Maybe.withDefault ""


lineLength : Array String -> Int -> Int
lineLength lines lineNum =
    lineContent lines lineNum
        |> String.length


selectedText : Selection -> Hover -> Array String -> String
selectedText selection currentHover lines =
    let
        positionsToString : Position -> Position -> String
        positionsToString from to =
            let
                numberOfLines =
                    to.line - from.line + 1
            in
            lines
                |> Array.toList
                |> List.drop from.line
                |> List.take numberOfLines
                |> List.indexedMap
                    (\i line ->
                        if numberOfLines == 1 then
                            line
                                |> String.dropLeft from.column
                                |> String.left (to.column - from.column + 1)

                        else if i == 0 then
                            String.dropLeft from.column line

                        else if i == numberOfLines - 1 then
                            String.left (to.column + 1) line

                        else
                            line
                    )
                |> String.join "\n"
    in
    case selection of
        NoSelection ->
            ""

        SelectingFrom startHover ->
            hoversToPositions lines startHover currentHover
                |> Maybe.map (\( from, to ) -> positionsToString from to)
                |> Maybe.withDefault ""

        SelectedChar { line, column } ->
            lineContent lines line
                |> String.dropLeft column
                |> String.left 1

        Selection from to ->
            positionsToString from to


view : Model -> Element.Element Msg
view model =
    Element.row
        [ Element.Font.size 18
        , Element.Font.family [ Element.Font.monospace ]
        , Element.width <| Element.px 500
        , Element.htmlAttribute <|
            HE.on "keydown"
                (JD.map2 Tuple.pair
                    (JD.field "ctrlKey" JD.bool)
                    (JD.field "key" JD.string)
                    |> JD.andThen keyToMsg
                )
        , Element.htmlAttribute <| HA.style "white-space" "pre"
        , Element.htmlAttribute <| HA.style "tab-size" "4"
        , Element.htmlAttribute <| HA.tabindex 0
        , Element.htmlAttribute <| HA.id "editor"
        ]
        [ viewLineNumbers model, viewContent model ]


viewLineNumbers : Model -> Element.Element Msg
viewLineNumbers model =
    let
        viewLineNumber n =
            Element.el
                [ Element.width <| Element.px 36 -- 2em
                , Element.Background.color <| Element.rgb255 128 128 128
                ]
            <|
                Element.el [ Element.centerX ] <|
                    Element.text (String.fromInt <| n - 1)
    in
    Element.column [ Element.width <| Element.px 36 ]
        (List.range 1 (Array.length model.lines)
            |> List.map viewLineNumber
        )


viewContent : Model -> Element.Element Msg
viewContent model =
    viewLines model.cursor model.hover model.selection model.lines
        |> Element.el
            [ Element.Background.color <| Element.rgb255 240 240 240
            , Element.width <| Element.fill
            , Element.Events.onMouseDown StartSelecting
            , Element.Events.onMouseUp StopSelecting

            -- , Element.Events.onClick GoToHoveredPosition
            -- , Element.Events.onMouseLeave (Hover NoHover)
            -- , Element.htmlAttribute <| HA.style "user-select" "none"
            ]


viewLines : Position -> Hover -> Selection -> Array String -> Element.Element Msg
viewLines position hover selection lines =
    let
        viewElLine line content =
            Element.el
                [ Element.Events.onMouseMove (Hover (HoverLine line))
                , Element.height <| Element.px 18
                ]
            <|
                Element.row [] <|
                    (content
                        |> String.toList
                        |> List.indexedMap (viewChar line)
                    )
                        ++ (if position.line == line && isLastColumn lines line position.column then
                                [ viewCursor position nbsp ]

                            else
                                []
                           )

        viewChar line column char =
            if position.line == line && position.column == column then
                viewCursor
                    position
                    (String.fromChar char)

            else if selection /= NoSelection && isSelected lines selection hover line column then
                viewSelectedChar
                    { line = line, column = column }
                    (String.fromChar char)

            else
                Element.text (String.fromChar char)
                    |> Element.el
                        [ onHover { line = line, column = column } ]
    in
    lines |> Array.indexedMap viewElLine |> Array.toList |> Element.column []


isSelected : Array String -> Selection -> Hover -> Int -> Int -> Bool
isSelected lines selection currentHover line column =
    let
        isSelectedPositions : Position -> Position -> Bool
        isSelectedPositions from to =
            (from.line <= line)
                && (to.line >= line)
                && (if from.line == line then
                        from.column <= column

                    else
                        True
                   )
                && (if to.line == line then
                        to.column >= column

                    else
                        True
                   )
    in
    case selection of
        NoSelection ->
            False

        SelectingFrom startHover ->
            hoversToPositions lines startHover currentHover
                |> Maybe.map (\( from, to ) -> isSelectedPositions from to)
                |> Maybe.withDefault False

        SelectedChar position ->
            position == { line = line, column = column }

        Selection from to ->
            isSelectedPositions from to


nbsp : String
nbsp =
    "\u{00A0}"


viewCursor : Position -> String -> Element.Element Msg
viewCursor position char =
    Element.text char
        |> Element.el
            [ Element.Background.color <| Element.rgb255 255 0 0 -- "orange"
            , onHover position
            ]


viewSelectedChar : Position -> String -> Element.Element Msg
viewSelectedChar position char =
    Element.text char
        |> Element.el
            [ Element.Background.color <| Element.rgb255 192 192 192 -- "ccc"
            , onHover position
            ]


onHover : Position -> Element.Attribute Msg
onHover position =
    Element.htmlAttribute <|
        HE.custom "mouseover"
            (JD.succeed
                { message = Hover (HoverChar position)
                , stopPropagation = True
                , preventDefault = True
                }
            )
