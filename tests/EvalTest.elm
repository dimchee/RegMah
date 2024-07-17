module EvalTest exposing (..)

import Array
import Eval
import Expect
import Lang
import Test exposing (..)


doTest : Test
doTest =
    describe "Sanity check"
        [ test "add 0 0\nadd 1 1"
            (\_ ->
                Lang.parse "add 0 0\nadd 1 1"
                    |> Result.withDefault Array.empty
                    |> Eval.eval
                    |> Expect.equal (Array.fromList [ Eval.recursionLimit, 0 ])
            )
        ]
