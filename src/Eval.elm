module Eval exposing (eval, recursionLimit)

import Array exposing (Array)
import Lang exposing (Instruction(..), Line, Program)


recursionLimit : number
recursionLimit =
    1000


type Progress
    = Running (Array Int) Line
    | Finished (Array Int)


evalLine : Line -> Array Int -> Program -> Progress
evalLine pc regs program =
    let
        modify i f =
            Array.set i (Array.get i regs |> Maybe.withDefault 0 |> f) regs

        next_branch i next_if next_else =
            Array.get i regs
                |> Maybe.map (always next_if)
                |> Maybe.withDefault next_else
    in
    case Array.get pc program of
        Just (Add { register, next }) ->
            Running (modify register ((+) 1)) next

        Just (Sub { register, next_if, next_else }) ->
            Running (modify register ((-) 1)) <| next_branch register next_if next_else

        Nothing ->
            Finished regs


eval : Program -> Array Int
eval program =
    let
        go nr line oldRegs =
            case evalLine line oldRegs program of
                Running regs next ->
                    -- TODO hard limit on number of steps
                    if nr < recursionLimit then
                        go (nr + 1) next regs

                    else
                        regs

                Finished regs ->
                    regs
    in
    Array.repeat
        (program
            |> Array.toList
            |> List.map Lang.getRegister
            |> List.maximum
            |> Maybe.withDefault 0
            |> (+) 1
        )
        0
        |> go 1 0
