module Explorer.Value.Expando where


type Expando
    = ExInt Int
    | ExFloat Toggle Float
    | ExChar Char
    | ExString Toggle String
    | ExBool Bool
    | ExRecord Toggle (List (String, Expando))
    | ExTag Toggle String (List Expando)
    | ExTuple (List Expando)
    | ExList Toggle (List Expando)
    | ExDict Toggle (List (Expando, Expando))
    | ExSet Toggle (List Expando)
    | ExArray Toggle (List Expando)
    | ExFunction String
    | ExBuiltIn String



-- TOGGLE


type Toggle
    = Show
    | Hide


showIf : Bool -> Toggle
showIf bool =
  if bool then Show else Hide



-- CREATE EXPANDOS


toExpando : ElmValue -> Expando
toExpando value =
  case value of
    VInt int ->
      ExInt int

    VFloat float ->
      ExFloat Hide float

    VChar char ->
      ExChar char

    VString string ->
      ExString (showIf (String.length string < 40)) string

    VBool bool ->
      ExBool bool

    VRecord fields ->
      ExRecord Hide (List.map (\(key, val) -> (key, toExpando val)) fields)

    VTag tag args ->
      ExTag Hide tag (List.map toExpando args)

    VTuple args ->
      ExTuple (List.map toExpando args)

    VList elements ->
      ExList Hide (List.map toExpando elements)

    VDict fields ->
      ExDict Hide (List.map (\(k,v) -> (toExpando k, toExpando v)) fields)

    VSet elements ->
      ExSet Hide (List.map toExpando elements)

    VArray elements ->
      ExArray Hide (List.map toExpando elements)

    VFunction name ->
      ExFunction name

    VBuiltIn name ->
      ExBuiltIn name



-- MERGE VALUES INTO AN EXISTING EXPANDO


merge : ElmValue -> Expando -> Expando
merge newValue existingValue =
  case (newValue, existingValue) of
    (VFloat float, ExFloat toggle _) ->
      ExFloat toggle float

    (VString str, ExString toggle _) ->
      ExString toggle str

    (VRecord fields, ExRecord toggle exFields) ->
      let
        exFieldsDict =
          Dict.fromList exFields

        mergeField (key, value) =
          case Dict.get key exFieldsDict of
            Nothing ->
              (key, toExpando value)

            Just exValue ->
              (key, merge value exValue)
      in
        ExRecord toggle (List.map mergeField fields)

    (VTag tag args, ExTag toggle exTag exArgs) ->
      let
        newExArgs =
          if tag == exTag then
            List.map2 merge args exArgs

          else
            List.map toExpando args
      in
        ExTag toggle tag newExArgs

    (VTuple elements, ExTuple exElements) ->
      ExTuple (mergeList elements exElements)

    (VList elements, ExList toggle exElements) ->
      ExList toggle (mergeList elements exElements)

    (VDict fields, ExDict toggle _) ->
      -- TODO: be more clever here
      toExpando newValue

    (VSet elements, ExSet toggle exElements) ->
      ExSet toggle (mergeList elements exElements)

    (VArray elements, ExArray toggle exElements) ->
      ExArray toggle (mergeList elements exElements)

    (_, _) ->
      toExpando newValue


mergeList : List ElmValue -> List Expando -> List Expando
mergeList values expandos =
  case (values, expandos) of
    (v :: vs, e :: es) ->
      merge v e :: mergeList vs es

    (_, []) ->
      List.map toExpando values

    ([], _) ->
      []


