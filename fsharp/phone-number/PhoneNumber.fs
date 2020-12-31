module PhoneNumber

open System
let remove (pat: string) (input: string) = input.Replace(pat, "")

// TODO: しっかり対処するには country code, area code, exchange code, subscriber number へ parse する必要がありそう
let prepare (input: string) =
    input
    |> remove " "
    |> remove "-"
    |> remove "."
    |> remove "("
    |> remove ")"

let removeFrontPlus (input: string) = input.TrimStart('+')

let checkLength (input: string) =
    match input.Length with
    | len when len < 10 -> Error "incorrect number of digits"
    | len when len > 11 -> Error "more than 11 digits"
    | _ -> Ok input

let removeCountryCode (input: string) =
    match input.Length with
    | 11 when input.StartsWith('1') -> Ok(input.Substring(1))
    | 11 -> Error "11 digits must start with 1"
    | _ -> Ok input

let checkNoLetters (input: string) =
    if input |> String.exists Char.IsLetter then Error "letters not permitted" else Ok input

let checkNoPunctuations (input: string) =
    if input |> String.exists Char.IsPunctuation
    then Error "punctuations not permitted"
    else Ok input

let checkAreaCode (input: string) =
    match input.[0] with
    | '0' -> Error "area code cannot start with zero"
    | '1' -> Error "area code cannot start with one"
    | _ -> Ok input

let checkExchangeCode (input: string) =
    match input.[3] with
    | '0' -> Error "exchange code cannot start with zero"
    | '1' -> Error "exchange code cannot start with one"
    | _ -> Ok input


let clean (input: string) =
    input
    |> prepare
    |> removeFrontPlus
    |> checkLength
    |> Result.bind removeCountryCode
    |> Result.bind checkNoLetters
    |> Result.bind checkNoPunctuations
    |> Result.bind checkAreaCode
    |> Result.bind checkExchangeCode
    |> Result.map uint64
