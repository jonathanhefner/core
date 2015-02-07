module Generator
    ( generator, take, foldl, foldr
    , map, filterMap, any
    , toList, fromList, repeat
    ) where

import Basics (..)
import Maybe
import Maybe (Maybe(Just, Nothing))
import List
import Native.Generator


intMax : Int
intMax = 2147483647 -- (2^31 - 1)


type alias RawGenerator a state =
  { next : state -> Maybe state
  , peek : state -> Maybe a
  , init : Maybe state
  }

type Opaque = Opaque

type alias Generator a = RawGenerator a Opaque


rawGenerator : (state -> Maybe state) -> (state -> Maybe a) -> state -> RawGenerator a state
rawGenerator next peek init =
  RawGenerator next peek (Just init)

elide : RawGenerator a state -> Generator a
elide = Native.Generator.elide

generator : (state -> Maybe state) -> (state -> Maybe a) -> state -> Generator a
generator next peek init =
  rawGenerator next peek init |> elide

take : Int -> Generator a -> List a
take = Native.Generator.take

foldl : (a -> b -> b) -> b -> Generator a -> b
foldl = Native.Generator.foldl

foldr : (a -> b -> b) -> b -> Generator a -> b
foldr = Native.Generator.foldr


map : (a -> b) -> Generator a -> Generator b
map f gen =
  { gen | peek <- (gen.peek >> Maybe.map f) }


filterMap : (a -> Maybe b) -> Generator a -> Generator b
filterMap f gen =
  let f' maybe =
        case maybe of
          Just x -> f x
          _ -> Nothing
  in
      { gen | peek <- (gen.peek >> f') }


any : (a -> Bool) -> Generator a -> Bool
any pred gen =
  let pred' x =
        if pred x
          then Just True
          else Nothing
  in
      filterMap pred' gen |> take 1 |> List.isEmpty |> not


toList : Generator a -> List a
toList gen =
  take intMax gen


fromList : List a -> Generator a
fromList xs =
  let next st =
        case st of
          _::tl -> Just tl
          _ -> Nothing
      peek st =
        case st of
          hd::_ -> Just hd
          _ -> Nothing
  in
      generator next peek xs


repeat : a -> Generator a
repeat x =
  generator (Just) (Just) x
