module Bob

open System

// ?
let isYell (s: string) =
    s
    |> Seq.filter (fun c -> System.Char.IsLower(c) || System.Char.IsUpper(c))
    |> Seq.forall (fun c -> System.Char.IsUpper(c))

let isQuestion (s: string) =
    s
    |> Seq.last = '?'

let isAllSeparator (s: string) =
    s |> Seq.forall (fun c -> System.Char.IsSeparator(c))

let response (input: string): string =
    if isYell input then
        if isQuestion input then
            "Calm down, I know what I'm doing!"
        else
            "Whoa, chill out!"
    else if isQuestion input then
        "Sure."
    else if isAllSeparator input then
        "Fine. Be that way!"
    else
        "Whatever."
