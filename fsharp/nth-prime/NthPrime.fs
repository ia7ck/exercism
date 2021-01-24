module NthPrime

let isPrime x =
    match x with
    | x when x <= 1 -> false
    | 2 -> true
    | x ->
        let u = int (sqrt (float x)) + 1
        seq { 2 .. u }
        |> Seq.exists (fun y -> x % y = 0)
        |> not

let prime nth: int option =
    Seq.initInfinite id
    |> Seq.filter isPrime
    |> Seq.take nth
    |> Seq.tryLast
