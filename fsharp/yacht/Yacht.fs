module Yacht

type Category =
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One
    | Two
    | Three
    | Four
    | Five
    | Six

let number die =
    match die with
    | One -> 1
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6

let count elem l =
    l
    |> List.filter (fun e -> e = elem)
    |> List.length

let ones dice = 1 * (count One dice)
let twos dice = 2 * (count Two dice)
let threes dice = 3 * (count Three dice)
let fours dice = 4 * (count Four dice)
let fives dice = 5 * (count Five dice)
let sixes dice = 6 * (count Six dice)

let fullHouse dice =
    let sum = dice |> List.sumBy number

    let dice = List.sort dice
    let first = List.head dice
    let (front, back) = List.partition (fun d -> d = first) dice

    if (front.Length = 2 && back.Length = 3)
       || (front.Length = 3 && back.Length = 2) then
        if List.forall (fun d -> d = List.head back) back
        then sum
        else 0
    else
        0

let fourOfAKind dice =
    let dice = List.sort dice
    let first = List.head dice
    let (front, back) = List.partition (fun d -> d = first) dice

    if front.Length = 5 then
        List.sumBy number (List.take 4 front)
    elif front.Length = 4 then
        List.sumBy number front
    elif front.Length = 1 then
        if List.forall (fun d -> d = List.head back) back
        then List.sumBy number back
        else 0
    else
        0

let littleStraight dice =
    let dice = dice |> List.sort |> List.distinct

    match dice with
    | [ One; Two; Three; Four; Five ] -> 30
    | [ One; Two; Three; Four; Five; _ ] -> 30
    | [ _; One; Two; Three; Four; Five ] -> 30
    | _ -> 0

let bigStraight dice =
    let dice = dice |> List.sort |> List.distinct

    match dice with
    | [ Two; Three; Four; Five; Six ] -> 30
    | [ Two; Three; Four; Five; Six; _ ] -> 30
    | [ _; Two; Three; Four; Five; Six ] -> 30
    | _ -> 0

let choice dice = List.sumBy number dice

let yacht dice =
    let allSame =
        List.forall (fun d -> d = List.head dice) dice

    if allSame then 50 else 0


let score category dice =
    let calculator =
        match category with
        | Ones -> ones
        | Twos -> twos
        | Threes -> threes
        | Fours -> fours
        | Fives -> fives
        | Sixes -> sixes
        | FullHouse -> fullHouse
        | FourOfAKind -> fourOfAKind
        | LittleStraight -> littleStraight
        | BigStraight -> bigStraight
        | Choice -> choice
        | Yacht -> yacht

    calculator dice
