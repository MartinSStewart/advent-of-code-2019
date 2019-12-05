module Day5Tests exposing (..)

import Array
import Day5
import Expect
import Test exposing (..)


all =
    describe "Day 5 tests" <|
        [ test "Example" <|
            \_ ->
                let
                    input =
                        Array.fromList [ 1101, 100, -1, 4, 0 ]

                    expected =
                        { memory = Array.fromList [ 1101, 100, -1, 4, 99 ], output = 0 }
                in
                Day5.part1 1 0 { memory = input, output = 0 } |> Expect.equal expected
        , test "Example 2" <|
            \_ ->
                let
                    input =
                        Array.fromList [ 1002, 4, 3, 4, 33 ]

                    expected =
                        { memory = Array.fromList [ 1002, 4, 3, 4, 99 ], output = 0 }
                in
                Day5.part1 1 0 { memory = input, output = 0 } |> Expect.equal expected
        ]
