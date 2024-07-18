module EvalTest exposing (..)

import Array
import Eval
import Expect
import Lang
import Test exposing (..)
import Zadaci



-- doTest : Test
-- doTest =
--     describe "Sanity check"
--         [ test "add 0 0\nadd 1 1"
--             (\_ ->
--                 Lang.parse "add 0 0\nadd 1 1"
--                     |> Result.withDefault Array.empty
--                     |> Eval.eval
--                     |> Expect.equal (Array.fromList [ Eval.recursionLimit, 0 ])
--             )
--         ]


passExpect : Zadaci.TestCase -> Lang.Program -> Expect.Expectation
passExpect { input, output } program =
    Eval.eval (Debug.log "input: " input) program |> Debug.log "eval: " |> Array.toList |> Expect.equalLists (Array.toList output)


passAllExpect : Int -> String -> Expect.Expectation
passAllExpect n str =
    case ( Array.get n Zadaci.zadaci, Lang.parse str ) of
        ( Just { testCases }, Ok program ) ->
            Expect.all (testCases |> List.map passExpect) program

        _ ->
            Expect.fail "Couldn't parse"


expectTrue : Bool -> Expect.Expectation
expectTrue b =
    if b then
        Expect.pass

    else
        Expect.fail "was False"


passTest : Test
passTest =
    describe "Checking if our example program passes testCases"
        [ test "isti"
            (\_ ->
                Zadaci.isti (Array.fromList [ 1, 0 ]) (Array.fromList [ 1, 0 ]) |> expectTrue
            )
        , test "isti2"
            (\_ ->
                Zadaci.isti (Array.fromList [ 1, 1 ]) (Array.fromList [ 1, 0 ]) |> not |> expectTrue
            )
        , test "isti3"
            (\_ ->
                Zadaci.isti (Array.fromList [ 1, 0 ]) (Array.fromList [ 1, 0, 1 ]) |> expectTrue
            )
        , test "zadatak 1"
            (\_ ->
                passAllExpect 0 "sub 3 1 1"
            )
        , test "zadatak 2"
            (\_ ->
                passAllExpect 1 "sub 1 1 2\nsub 2 3 4\nadd 1 2"
            )
        ]
