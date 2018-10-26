module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P exposing ((|.), (|=))
import Parser.Maybe as M
import Test exposing (..)


suite : Test
suite =
    describe "The Parser.Maybe module"
        [ describe "maybe"
            -- Nest as many descriptions as you like.
            [ test "maybe int" <|
                \() ->
                    P.run (M.maybe P.int) "3"
                        |> Expect.equal (Ok (Just 3))
            , test "maybe not an int" <|
                \() ->
                    P.run (M.maybe P.int) "a"
                        |> Expect.equal (Ok Nothing)
            , test "maybe not an int, so it is zero" <|
                \() ->
                    P.run (M.withDefault 0 P.int) "a"
                        |> Expect.equal (Ok 0)
            , test "maybe an int, 2" <|
                \() ->
                    P.run (M.withDefault 0 P.int) "2"
                        |> Expect.equal (Ok 2)
            , test "maybe a tuple!" <|
                \() ->
                    P.run (M.maybe point) "2 2"
                        |> Expect.equal (Ok (Just ( 2, 2 )))
            , test "maybe not a tuple!" <|
                \() ->
                    P.run (M.maybe point) "2 a"
                        |> Expect.equal (Ok Nothing)
            , test "maybe not a tuple, in which case we make it (0,0)!" <|
                \() ->
                    P.run (M.withDefault ( 0, 0 ) point) "2 a"
                        |> Expect.equal (Ok ( 0, 0 ))
            , test "maybe a tuple between two ints - nothing" <|
                \() ->
                    P.run optionalPointBetweenTwoInts "23 23"
                        |> Expect.equal (Ok Nothing)
            , test "maybe a tuple between two ints - nothing again FAILS TEST" <|
                \() ->
                    P.run optionalPointBetweenTwoInts "23 21 23"
                        |> Expect.equal (Ok Nothing)
            , test "maybe a tuple between two ints - just " <|
                \() ->
                    P.run optionalPointBetweenTwoInts "23 21 21 23"
                        |> Expect.equal (Ok (Just ( 21, 21 )))
            ]
        ]


point : P.Parser ( Int, Int )
point =
    P.succeed Tuple.pair
        |= P.int
        |. P.spaces
        |= P.int


optionalPointBetweenTwoInts : P.Parser (Maybe ( Int, Int ))
optionalPointBetweenTwoInts =
    P.succeed identity
        |. P.int
        |. P.spaces
        |= M.maybe point
        |. P.spaces
        |. P.int
