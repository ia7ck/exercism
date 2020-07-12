module TwoFer

open System

let twoFer (input: string option): string =
    let name =
        match input with
        | Some name -> name
        | None -> "you"
    String.Format("One for {}, one for me.", name)
