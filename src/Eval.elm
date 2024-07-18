module Eval exposing (Registers, eval, extendRegisters, getRegister, recursionLimit, setRegister, toIndexedRegisterList)

import Array exposing (Array)
import Lang exposing (Instruction(..), Line, Program)


recursionLimit : number
recursionLimit =
    5000


type Progress
    = Running Registers Line
    | Finished Registers


type alias Registers =
    Array Int


setRegister : Int -> a -> Array a -> Array a
setRegister i x =
    Array.set (i - 1) x


getRegister : Int -> Array a -> Maybe a
getRegister i x =
    Array.get (i - 1) x


toIndexedRegisterList : Array a -> List ( Int, a )
toIndexedRegisterList =
    Array.toIndexedList >> List.map (Tuple.mapFirst ((+) 1))


evalLine : Line -> Registers -> Program -> Progress
evalLine pc regs program =
    let
        modify reg f =
            setRegister reg (getRegister reg regs |> Maybe.withDefault 0 |> f) regs
    in
    case Array.get (pc - 1) program of
        Just (Add { register, next }) ->
            Running (modify register ((+) 1)) next

        Just (Sub { register, next_if, next_else }) ->
            case getRegister register regs of
                Nothing ->
                    Running regs next_else

                Just 0 ->
                    Running regs next_else

                _ ->
                    Running (modify register (\x -> x - 1)) next_if

        Nothing ->
            Finished regs


extendRegisters : Array (Maybe Int) -> Int -> Array (Maybe Int)
extendRegisters regs n =
    if n > Array.length regs then
        Array.append regs <| Array.repeat (n - Array.length regs) Nothing

    else
        Array.slice 0 n regs


eval : Array.Array (Maybe Int) -> Program -> Registers
eval initRegs program =
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

        maxReg =
            program
                |> Array.toList
                |> List.map Lang.getRegister
                |> List.maximum
                |> Maybe.withDefault 0
    in
    extendRegisters initRegs
        (if maxReg < 1000 then
            maxReg

         else
            0
        )
        |> Array.map (Maybe.withDefault 0)
        |> go 1 1
