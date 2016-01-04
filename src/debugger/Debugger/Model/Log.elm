module Debugger.Model.Log
    ( Log, empty, fromList, toList, append, last
    , mostRecentBeforeIndex, truncate, map, filter, isEmpty, update
    ) where


import Utils.Helpers


type Log timestamp item =
  Log (List (timestamp, item))


empty : Log timestamp item
empty =
  Log []


fromList : List (timestamp, item) -> Log timestamp item
fromList log =
  Log log


toList : Log timestamp item -> List (timestamp, item)
toList (Log log) =
  log


isEmpty : Log timestamp item -> Bool
isEmpty (Log log) =
  List.isEmpty log


append : (timestamp, item) -> Log timestamp item -> Log timestamp item
append pair (Log log) =
  log ++ [pair] |> Log


last : Log timestamp item -> Maybe (timestamp, item)
last (Log log) =
  log |> Utils.Helpers.last


mostRecentBeforeIndex : comparable -> Log comparable item -> Maybe item
mostRecentBeforeIndex timestamp (Log log) =
  log
    |> List.filter (\(itemIdx, val) -> itemIdx <= timestamp)
    |> Utils.Helpers.last
    |> Maybe.map snd


truncate : comparable -> Log comparable item -> Log comparable item
truncate timestamp (Log log) =
  List.filter (\(idx, val) -> idx <= timestamp) log |> Log


map : ((timestamp, item) -> (timestamp, b)) -> Log timestamp item -> Log timestamp b
map func (Log log) =
  List.map func log |> Log


filter : ((timestamp, item) -> Bool) -> Log timestamp item -> Log timestamp item
filter predicate (Log log) =
  List.filter predicate log |> Log


update
  : comparable
  -> ((comparable, item) -> item)
  -> Log comparable item
  -> Maybe (Log comparable item)
update timestamp updater (Log log) =
  let
    (before, after) =
      List.partition (\(ts, _) -> ts < timestamp) log
  in
    case after of
      [] ->
        Nothing

      ((ts, x)::xs) ->
        before ++ ((ts, updater (ts, x)) :: xs)
          |> Log
          |> Just
