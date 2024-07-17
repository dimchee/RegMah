module Lang exposing (Instruction(..), Line, Program, getRegister, parse)

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Step(..), chompWhile, int, loop, oneOf, succeed, symbol)


type alias Line =
    Int


type alias SepList a sep =
    ( a, List ( sep, a ) )


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\u{000D}')


type alias AddArgs =
    { register : Int, next : Line }


type alias SubArgs =
    { register : Int, next_if : Line, next_else : Line }


type Instruction
    = Add AddArgs
    | Sub SubArgs


getRegister : Instruction -> Int
getRegister inst =
    case inst of
        Add { register } ->
            register

        Sub { register } ->
            register


instruction : Parser Instruction
instruction =
    oneOf
        [ Parser.map Add <|
            succeed AddArgs
                |. symbol "add"
                |. spaces
                |= int
                |. spaces
                |= int
        , Parser.map Sub <|
            succeed SubArgs
                |. symbol "sub"
                |. spaces
                |= int
                |. spaces
                |= int
                |. spaces
                |= int
        ]


step : List ( sep, a ) -> a -> Maybe sep -> Step (List ( sep, a )) (SepList a sep)
step revOps expr nextOp =
    case nextOp of
        Nothing ->
            Done ( expr, revOps )

        Just op ->
            Loop <| ( op, expr ) :: revOps


chain : Parser a -> Parser b -> Parser (SepList a b)
chain parseA parseB =
    loop [] <|
        \revOps ->
            succeed (step revOps)
                |= parseA
                |. spaces
                |= oneOf
                    [ succeed Just
                        |= parseB
                        |. spaces
                    , succeed Nothing
                    ]


sepBy : String -> Parser keep -> Parser (List keep)
sepBy sep parser =
    Parser.map (\( x, xs ) -> x :: List.map Tuple.second xs) <| chain parser (symbol sep)


type alias Program =
    Array Instruction


program : Parser Program
program =
    Parser.map (Array.fromList << List.reverse) <| sepBy "\n" instruction |. spaces |. Parser.end


parse : String -> Result (List Parser.DeadEnd) Program
parse =
    Parser.run program
