module Wordy

// 累乗の parse には対応してない

//#r "nuget: FParsec"
open FParsec

let ws = spaces
let num_ws = ws >>. pint32 .>> ws
let str_return_ws s a = ws >>. stringReturn s a .>> ws

let operator: Parser<int -> int -> int, unit> =
    str_return_ws "plus" (+)
    <|> str_return_ws "minus" (-)
    <|> str_return_ws "multiplied by" (*)
    <|> str_return_ws "divided by" (/)

let chain_op = chainl1 num_ws operator

let parser =
    pstring "What is" >>. chain_op .>> pstring "?"

let answer question =
    match run parser question with
    | Success (result, _, _) -> Some(result)
    | Failure (msg, _, _) ->
        printfn "%s" msg
        None
