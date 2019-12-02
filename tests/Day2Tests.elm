module Day2Tests exposing (..)

import Array
import Day2
import Expect
import Test exposing (..)


all =
    describe "Day 2 tests" <|
        [ test "Example" <|
            \_ ->
                let
                    input =
                        Array.fromList [ 1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50 ]

                    expected =
                        [ 3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50 ]
                in
                Day2.part1 0 input |> Array.toList |> Expect.equal expected
        ]
