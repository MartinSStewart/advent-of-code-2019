module Day10Tests exposing (..)

import Array
import Day10
import Expect
import Test exposing (..)


exampleInput =
    """......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"""


all =
    describe "Day 10 tests" <|
        [ only <|
            test "Example" <|
                \_ ->
                    Day10.part1 exampleInput |> Expect.equal 33
        ]
