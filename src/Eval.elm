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
    Array (Maybe Int)


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
            setRegister reg (getRegister reg regs |> Maybe.andThen (Maybe.map f)) regs
    in
    case Array.get (pc - 1) program of
        Just (Add { register, next }) ->
            Running (modify register ((+) 1)) next

        Just (Sub { register, next_if, next_else }) ->
            case getRegister register regs of
                Nothing ->
                    Running regs next_else

                Just (Just 0) ->
                    Running regs next_else

                Just Nothing ->
                    Running regs next_else

                _ ->
                    Running (modify register (\x -> x - 1)) next_if

        Nothing ->
            Finished regs


extendRegisters : Array (Maybe Int) -> List Int -> Array (Maybe Int)
extendRegisters regs inds =
    let
        n =
            List.maximum inds |> Maybe.withDefault 0

        noths _ =
            Array.repeat (n - Array.length regs) Nothing |> Array.append regs
    in
    if n > Array.length regs then
        List.foldl (\i arr -> Array.set i (Just 0) arr) (noths ()) inds

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

        inds =
            program
                |> Array.toList
                |> List.map Lang.getRegister
    in
    extendRegisters initRegs
        (if Maybe.withDefault 0 (List.maximum inds) < 1000 then
            inds

         else
            []
        )
        |> go 1 1
