module Test.Generator (tests) where

import ElmTest.Assertion (..)
import ElmTest.Test (..)

import Basics (..)
import Maybe (Maybe(Nothing, Just))
import List
import List ((::))
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
      one = List.take 1 xs
      xsSum = n * (n + 1) // 2
      nonzero x =
        if x == 0
          then Nothing
          else Just x
  in
      suite (toString n ++ " items")
        [ suite "toList"
            [ test "all" <| assertEqual (xs) (toList genX)
            , test "limit -1" <| assertEqual ([]) (limit -1 genX |> toList)
            , test "limit 0" <| assertEqual ([]) (limit 0 genX |> toList)
            , test "limit 1" <| assertEqual (one) (limit 1 genX |> toList)
            , test "limit n" <| assertEqual (xs) (limit n genX |> toList)
            , test "limit n + 1" <| assertEqual (xs) (limit (n + 1) genX |> toList)
            ]

        , suite "foldl"
            [ test "order" <| assertEqual (n) (foldl (\x acc -> x) 0 genX)
            , test "total" <| assertEqual (xsSum) (foldl (+) 0 genX)
            , test "limit -1" <| assertEqual ([]) (limit -1 genX |> foldl (::) [])
            , test "limit 0" <| assertEqual ([]) (limit 0 genX |> foldl (::) [])
            , test "limit 1" <| assertEqual (one) (limit 1 genX |> foldl (::) [])
            ]

        , suite "foldr"
            [ test "order" <| assertEqual (min 1 n) (foldr (\x acc -> x) 0 genX)
            , test "total" <| assertEqual (xsSum) (foldr (+) 0 genX)
            , test "limit -1" <| assertEqual ([]) (limit -1 genX |> foldr (::) [])
            , test "limit 0" <| assertEqual ([]) (limit 0 genX |> foldr (::) [])
            , test "limit 1" <| assertEqual (one) (limit 1 genX |> foldr (::) [])
            ]

        , test "take" <| assertEqual (xs) (take n genX)

        , test "map" <| assertEqual (xs) (map (\x -> x + 1) genZ |> take n)

        , test "filterMap" <| assertEqual (xs) (filterMap nonzero genZ |> toList)

        , suite "any"
            [ test "false" <| assertEqual (False) (any (\x -> x > n) genX)
            , test "true" <| assertEqual (True) (any (\z -> z >= n) genZ)
            ]
        ]
