module DataUtils where

import Debug

{-| Would call this Utils except I don't want conflicts with the module
being debugged (this is a problem for all modules in the debugger) -}

getLast : List a -> a
getLast list =
  case list of
    [] ->
      Debug.crash "getLast of empty list"

    [x] ->
      x

    (x::xs) ->
      getLast xs


getMaybe : String -> Maybe a -> a
getMaybe msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg
