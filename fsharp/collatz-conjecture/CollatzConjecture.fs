module CollatzConjecture

let rec f n cnt =
    if n = 1 then cnt
    else if n % 2 = 0 then f (n / 2) (cnt + 1)
    else f (n * 3 + 1) (cnt + 1)

let steps (number: int): int option =
    if number <= 0 then None else Some(f number 0)
