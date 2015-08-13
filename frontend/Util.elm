module Util ( (</>), takeExtension ) where

import String


(</>) : String -> String -> String
(</>) directory file =
  if String.endsWith "/" directory then
    directory ++ file
  else
    directory ++ "/" ++ file


takeExtension : String -> String
takeExtension str =
  last (String.split "." str)


last : List String -> String
last list =
  case list of
    [] ->
      ""

    [finalElement] ->
      finalElement

    _ :: rest ->
      last rest
