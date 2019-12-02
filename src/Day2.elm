module Day2 exposing (part1, part1Output, part2Output)

import Array
import List.Extra as List


input =
    """1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0"""


parsedInput =
    input |> String.split "," |> List.filterMap String.toInt |> Array.fromList


part1 index values =
    let
        opCode =
            Array.get index values |> Maybe.withDefault 0

        value0Addr =
            Array.get (index + 1) values |> Maybe.withDefault 0

        value1Addr =
            Array.get (index + 2) values |> Maybe.withDefault 0

        outputAddr =
            Array.get (index + 3) values |> Maybe.withDefault 0

        value0 =
            Array.get value0Addr values |> Maybe.withDefault 0

        value1 =
            Array.get value1Addr values |> Maybe.withDefault 0
    in
    if opCode == 1 then
        Array.set outputAddr (value0 + value1) values |> part1 (index + 4)

    else if opCode == 2 then
        Array.set outputAddr (value0 * value1) values |> part1 (index + 4)

    else if opCode == 99 then
        values

    else
        values


part1Output =
    parsedInput
        |> Array.set 1 12
        |> Array.set 2 2
        |> part1 0
        |> Array.get 0
        |> Maybe.withDefault 0
        |> String.fromInt


part2 noun verb =
    parsedInput
        |> Array.set 1 noun
        |> Array.set 2 verb
        |> part1 0
        |> Array.get 0
        |> Maybe.withDefault 0


part2Output =
    List.range 0 (99 * 99)
        |> List.map
            (\value ->
                let
                    noun =
                        modBy 99 value

                    verb =
                        value // 99
                in
                ( noun, verb, part2 noun verb )
            )
        |> List.filterMap
            (\( noun, verb, output ) ->
                if output == 19690720 then
                    (noun * 100) + verb |> Just

                else
                    Nothing
            )
        |> List.head
        |> Maybe.withDefault 0
        |> String.fromInt
