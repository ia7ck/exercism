module Gigasecond

open System

let add (beginDate: DateTime) = beginDate.AddSeconds(pown 10 9 |> float)
