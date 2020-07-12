module QueenAttack

let create (position: int * int): bool =
    let (x, y) = position
    0 <= x && x < 8 && 0 <= y && y < 8

let canAttack (queen1: int * int) (queen2: int * int): bool =
    let (x1, y1) = queen1
    let (x2, y2) = queen2
    x1 = x2 || y1 = y2 || abs (x1 - x2) = abs (y1 - y2)
