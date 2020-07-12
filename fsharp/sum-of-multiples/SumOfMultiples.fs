module SumOfMultiples

let sum (numbers: int list) (upperBound: int): int =
    [ 0 .. upperBound - 1 ]
    |> List.filter (fun a ->
        numbers
        |> List.exists (fun b ->
            if b = 0 then a = 0 else a % b = 0))
    |> List.sum
