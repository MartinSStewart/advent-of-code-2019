module Day5 exposing (input, parsedInput, part1, part1Output)

import Array exposing (Array)
import List.Extra as List


input =
    """3,225,1,225,6,6,1100,1,238,225,104,0,1102,46,47,225,2,122,130,224,101,-1998,224,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,1102,61,51,225,102,32,92,224,101,-800,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,61,64,225,1001,118,25,224,101,-106,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,33,25,225,1102,73,67,224,101,-4891,224,224,4,224,1002,223,8,223,1001,224,4,224,1,224,223,223,1101,14,81,225,1102,17,74,225,1102,52,67,225,1101,94,27,225,101,71,39,224,101,-132,224,224,4,224,1002,223,8,223,101,5,224,224,1,224,223,223,1002,14,38,224,101,-1786,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1,65,126,224,1001,224,-128,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1101,81,40,224,1001,224,-121,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,677,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,344,101,1,223,223,1107,677,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,374,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,419,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,434,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,449,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,1007,677,226,224,1002,223,2,223,1006,224,479,1001,223,1,223,1007,677,677,224,1002,223,2,223,1005,224,494,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,524,1001,223,1,223,7,226,226,224,102,2,223,223,1005,224,539,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,554,101,1,223,223,107,677,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,226,677,224,1002,223,2,223,1005,224,584,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,614,101,1,223,223,7,677,226,224,102,2,223,223,1005,224,629,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,659,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"""


parsedInput =
    input |> String.split "," |> List.filterMap String.toInt |> Array.fromList


unsafeGet index array =
    Array.get index array |> Maybe.withDefault 0


part1 index values =
    let
        { opCode, firstParameter, secondParameter, thirdParameter } =
            Array.get index values |> Maybe.withDefault 0 |> decodeInstruction

        value0Addr =
            getMemory firstParameter (index + 1) values

        value1Addr =
            getMemory secondParameter (index + 2) values

        outputAddr =
            getMemory thirdParameter (index + 3) values

        value0 =
            Array.get value0Addr values |> Maybe.withDefault 0

        value1 =
            Array.get value1Addr values |> Maybe.withDefault 0
    in
    if opCode == 1 then
        Array.set outputAddr (value0 + value1) values |> part1 (index + 4)

    else if opCode == 2 then
        Array.set outputAddr (value0 * value1) values |> part1 (index + 4)

    else if opCode == 3 then
        Array.set (unsafeGet (index + 1) values) (unsafeGet (index + 1) values) |> part1 (index + 1)

    else if opCode == 99 then
        values

    else
        values


getMemory : Mode -> Int -> Array Int -> Int
getMemory mode index values =
    case mode of
        PositionMode ->
            Array.get index values |> Maybe.withDefault 0

        ImmediateMode ->
            index


unsafeInt text =
    String.toInt text |> Maybe.withDefault 0


type Mode
    = PositionMode
    | ImmediateMode


decodeInstruction value =
    case value |> String.fromInt |> String.toList of
        a :: b :: c :: d :: e :: [] ->
            decodeInstructionHelper d e c b a

        a :: b :: c :: d :: [] ->
            decodeInstructionHelper c d b a '0'

        a :: b :: c :: [] ->
            decodeInstructionHelper b c a '0' '0'

        _ ->
            { opCode = value
            , firstParameter = PositionMode
            , secondParameter = PositionMode
            , thirdParameter = PositionMode
            }


decodeInstructionHelper a b c d e =
    { opCode = a :: [ b ] |> String.fromList |> unsafeInt
    , firstParameter = decodeMode c
    , secondParameter = decodeMode d
    , thirdParameter = decodeMode e
    }


decodeMode value =
    if value == '0' then
        PositionMode

    else if value == '1' then
        ImmediateMode

    else
        ImmediateMode


part1Output =
    parsedInput
        |> Array.set 1 12
        |> Array.set 2 2
        |> part1 0
        |> Array.get 0
        |> Maybe.withDefault 0
        |> String.fromInt
