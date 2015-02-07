module Test.Generator (tests) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Basics (..)
import Maybe (Maybe(Nothing, Just))
import List
import Generator (..)


tests : Test
tests = suite "Generator Tests"
  [ testGeneratorOfN 0
  , testGeneratorOfN 1
  , testGeneratorOfN 2
  , testGeneratorOfN 1000
  ]


testGeneratorOfN : Int -> Test
testGeneratorOfN n =
  let xs = [1..n]
      zs = [0..n]
      genX = fromList xs
      genZ = fromList zs
      xsSum = n * (n + 1) // 2
      nonzero x =
        if x == 0
          then Nothing
          else Just x
  in
      suite (toString n ++ " items")
        [ test "toList" <| assertEqual (xs) (toList genX)

        , suite "take"
            [ test "-1" <| assertEqual ([]) (take -1 genX)
            , test "0" <| assertEqual ([]) (take 0 genX)
            , test "1" <| assertEqual (List.take 1 xs) (take 1 genX)
            , test "n" <| assertEqual (xs) (take n genX)
            , test "n + 1" <| assertEqual (xs) (take (n + 1) genX)
            ]

        , suite "foldl"
            [ test "order" <| assertEqual (n) (foldl (\x acc -> x) 0 genX)
            , test "total" <| assertEqual (xsSum) (foldl (+) 0 genX)
            ]

        , suite "foldr"
            [ test "order" <| assertEqual (min 1 n) (foldr (\x acc -> x) 0 genX)
            , test "total" <| assertEqual (xsSum) (foldr (+) 0 genX)
            ]

        , test "map" <| assertEqual (xs) (map (\x -> x + 1) genZ |> take n)

        , test "filterMap" <| assertEqual (xs) (filterMap nonzero genZ |> toList)

        , suite "any"
            [ test "false" <| assertEqual (False) (any (\x -> x > n) genX)
            , test "true" <| assertEqual (True) (any (\z -> z >= n) genZ)
            ]
        ]
