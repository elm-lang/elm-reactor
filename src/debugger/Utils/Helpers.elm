module Utils.Helpers where


{- Would call this Utils except I don't want conflicts with the module
being debugged (this is a problem for all modules in the debugger)
-}

last : List a -> Maybe a
last list =
  case list of
    [] ->
      Nothing

    [x] ->
      Just x

    x::xs ->
      last xs


unsafe : String -> Maybe a -> a
unsafe msg maybe =
  case maybe of
    Just x ->
      x

    Nothing ->
      Debug.crash msg


unsafeResult : Result String r -> r
unsafeResult result =
  case result of
    Ok val ->
      val

    Err msg ->
      Debug.crash msg


maybeToList : Maybe a -> List a
maybeToList maybe =
  case maybe of
    Just x ->
      [x]

    Nothing ->
      []


