module Day10 exposing (part1, part1Output)

import Arithmetic
import Set exposing (Set)


input =
    """#..#.#.#.######..#.#...##
##.#..#.#..##.#..######.#
.#.##.#..##..#.#.####.#..
.#..##.#.#..#.#...#...#.#
#...###.##.##..##...#..#.
##..#.#.#.###...#.##..#.#
###.###.#.##.##....#####.
.#####.#.#...#..#####..#.
.#.##...#.#...#####.##...
######.#..##.#..#.#.#....
###.##.#######....##.#..#
.####.##..#.##.#.#.##...#
##...##.######..##..#.###
...###...#..#...#.###..#.
.#####...##..#..#####.###
.#####..#.#######.###.##.
#...###.####.##.##.#.##.#
.#.#.#.#.#.##.#..#.#..###
##.#.####.###....###..##.
#..##.#....#..#..#.#..#.#
##..#..#...#..##..####..#
....#.....##..#.##.#...##
.##..#.#..##..##.#..##..#
.##..#####....#####.#.#.#
#..#..#..##...#..#.#.#.##"""


width =
    26


type alias Position =
    ( Int, Int )


parseInput : String -> Set ( Int, Int )
parseInput =
    String.split "\n"
        >> List.indexedMap
            (\y row ->
                String.toList row
                    |> List.indexedMap
                        (\x char ->
                            if char == '#' then
                                Just ( x, y )

                            else
                                Nothing
                        )
                    |> List.filterMap identity
            )
        >> List.concat
        >> Set.fromList


part1 input_ =
    let
        parsedInput =
            parseInput input_
    in
    parsedInput
        |> Set.toList
        |> List.map (\pos -> countVisibleAsteroids 1 (parsedInput |> Set.remove pos) pos)
        |> List.maximum
        |> Maybe.withDefault -1
        |> (+) -1


part1Output =
    part1 input |> String.fromInt


removedHiddenAsteroids : Set ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Set ( Int, Int )
removedHiddenAsteroids set ( ax, ay ) ( vx, vy ) =
    let
        ( rx, ry ) =
            ( ax - vx, ay - vy ) |> simplify
    in
    List.range 1 (width - 1)
        |> List.map (\value -> ( vx + rx * value, vy + ry * value ))
        |> List.foldl Set.remove set


simplify ( x, y ) =
    let
        divisor =
            Arithmetic.gcd x y
    in
    ( x // divisor, y // divisor )


countVisibleAsteroids : Int -> Set ( Int, Int ) -> ( Int, Int ) -> Int
countVisibleAsteroids count set viewPosition =
    case Set.toList set |> List.head of
        Just head ->
            let
                newSet =
                    removedHiddenAsteroids set head viewPosition
            in
            countVisibleAsteroids (count + 1) newSet viewPosition

        Nothing ->
            count
