module Util where


import String


isSuffixOf = String.endsWith


(</>) : String -> String -> String
a </> b =
  if "/" `isSuffixOf` a
    then a ++ b
    else a ++ "/" ++ b


splitPath = String.split "/"


find : a -> List (a, b) -> Maybe b
find elem l =
  case l of
    [] -> Nothing
    (elem2, val)::tail ->
      if elem2 == elem
        then Just val
        else find elem tail


takeExtension : String -> String
takeExtension str =
  let
    loop l =
      case l of
        [] -> ""
        [a] -> a
        (_::t) -> loop t
  in
    case String.split "." str of
      [] -> ""
      (_::t) -> loop t


(><) : List a -> List b -> List (a,b)
(><) = List.map2 (,)
