module Generator
    ( generator, toList, foldl, foldr
    , limit, take, map, filterMap, any
    , fromList, repeat, nil, empty
    ) where

import Basics (..)
import Maybe
import Maybe (Maybe(Just, Nothing))
import Native.Generator


intMax : Int
intMax = 2147483647 -- (2^31 - 1)


type alias RawGenerator a state =
  { next : state -> Maybe state
  , peek : state -> Maybe a
  , init : Maybe state
  , limit : Int
  }

type Opaque = Opaque

type alias Generator a = RawGenerator a Opaque


rawGenerator : (state -> Maybe state) -> (state -> Maybe a) -> state -> RawGenerator a state
rawGenerator next peek init =
  RawGenerator next peek (Just init) intMax

elide : RawGenerator a state -> Generator a
elide = Native.Generator.elide

generator : (state -> Maybe state) -> (state -> Maybe a) -> state -> Generator a
generator next peek init =
  rawGenerator next peek init |> elide

toList : Generator a -> List a
toList = Native.Generator.toList

foldl : (a -> b -> b) -> b -> Generator a -> b
foldl = Native.Generator.foldl

foldr : (a -> b -> b) -> b -> Generator a -> b
foldr = Native.Generator.foldr


limit : Int -> Generator a -> Generator a
limit n gen =
  { gen | limit <- n }


take : Int -> Generator a -> List a
take n gen =
  gen |> limit n |> toList


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
      case (filterMap pred' gen |> take 1) of
        [] -> False
        _ -> True


fromList : List a -> Generator a
fromList xs =
  let next st =
        case st of
          _ :: tl -> Just tl
          _ -> Nothing
      peek st =
        case st of
          hd :: _ -> Just hd
          _ -> Nothing
  in
      generator next peek xs


repeat : a -> Generator a
repeat x =
  generator (Just) (Just) x


nil : Generator ()
nil = repeat ()


empty : Generator ()
empty = nil |> limit 0
