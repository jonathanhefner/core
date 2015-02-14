module Generator
    ( generator, toList, foldl, foldr
    , limit, take, map, filterMap, any
    , fromList, range, repeat, nil, empty
    ) where

import Basics (..)
import Maybe
import Maybe (Maybe(Just, Nothing))
import Native.Generator


intMax : Int
intMax = 2147483647 -- (2^31 - 1)


type alias RawGenerator a state =
  { next : state -> state
  , peek : state -> Maybe a
  , limit : Int
  , until : state -- ONLY "===" comparable!
  , start : state
  }

type Opaque = Opaque

type alias Generator a = RawGenerator a Opaque


rawGenerator : (state -> state) -> (state -> Maybe a) -> state -> state -> RawGenerator a state
rawGenerator next peek until start =
  RawGenerator next peek intMax until start

elide : RawGenerator a state -> Generator a
elide = Native.Generator.elide


fromList : List a -> Generator a
fromList xs =
  let next st =
        case st of
          _ :: tl -> tl
          _ -> st -- impossible
      peek st =
        case st of
          hd :: _ -> Just hd
          _ -> Nothing -- impossible
  in
      rawGenerator next peek [] xs |> elide


range : Int -> Int -> Generator Int
range a b =
  let step = if a < b then 1 else -1
  in
    rawGenerator ((+) step) (Just) b a |> elide


generator : (state -> Maybe state) -> (state -> Maybe a) -> state -> Generator a
generator next peek start =
  let maybeFlatMap f maybe =
        case maybe of
          Just x -> f x
          Nothing -> Nothing
      next' = maybeFlatMap next
      peek' = maybeFlatMap peek
  in
      rawGenerator next' peek' Nothing (Just start) |> elide


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


repeat : a -> Generator a
repeat x =
  generator (Just) (Just) x


nil : Generator ()
nil = repeat ()


empty : Generator ()
empty = nil |> limit 0
