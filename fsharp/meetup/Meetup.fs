module Meetup

open System

type Week =
    | First
    | Second
    | Third
    | Fourth
    | Last
    | Teenth

let meetup year month week dayOfWeek: DateTime =
    let dates =
        [ 1 .. DateTime.DaysInMonth(year, month) ]
        |> List.map (fun day -> DateTime(year, month, day))
        |> List.filter (fun date -> date.DayOfWeek = dayOfWeek)

    match week with
    | First -> List.item 0 dates
    | Second -> List.item 1 dates
    | Third -> List.item 2 dates
    | Fourth -> List.item 3 dates
    | Last -> List.last dates
    | Teenth ->
        let teenth =
            dates |> List.filter (fun date -> date.Day >= 13)

        List.head teenth
