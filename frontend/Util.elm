module Util where

import String


(</>) : String -> String -> String
(</>) directory file =
  if String.endsWith "/" directory then
    directory ++ file
  else
    directory ++ "/" ++ file


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
