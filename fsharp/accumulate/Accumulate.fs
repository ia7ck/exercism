module Accumulate

let accumulate (func : 'a -> 'b) (input : 'a list) : 'b list =
    let rec g l acc =
        match l with
        | head :: tail -> g tail ([func head] @ acc)
        | [] -> acc |> List.rev
    g input []
