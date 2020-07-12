module ArmstrongNumbers

let rec pow a x =
    if x = 0 then 1 else a * pow a (x - 1)

let isArmstrongNumber (number: int): bool =
    let s = string number
    let len = s.Length

    let sum =
        s
        |> Seq.sumBy (fun c ->
            let d = int c - int '0'
            pow d len)
    number = sum
