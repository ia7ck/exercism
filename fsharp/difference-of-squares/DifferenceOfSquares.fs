module DifferenceOfSquares

let squareOfSum (number: int): int =
    let s = [ 1 .. number ] |> List.sum
    s * s

let sumOfSquares (number: int): int = [ 1 .. number ] |> List.sumBy (fun x -> x * x)

let differenceOfSquares (number: int): int = (squareOfSum number) - (sumOfSquares number)
