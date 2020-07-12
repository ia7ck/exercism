module RobotSimulator

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Robot =
    { Direction: Direction
      Position: Position }

let create direction position =
    { Direction = direction
      Position = position }

let advance dir pos =
    let (x, y) = pos
    match dir with
    | North -> (x, y + 1)
    | East -> (x + 1, y)
    | South -> (x, y - 1)
    | West -> (x - 1, y)

let turnRight dir =
    match dir with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let turnLeft dir =
    match dir with
    | North -> West
    | West -> South
    | South -> East
    | East -> North

let move instructions robot =
    instructions
    |> Seq.fold (fun ro c ->
        match c with
        | 'A' -> { ro with Position = advance ro.Direction ro.Position }
        | 'R' -> { ro with Direction = turnRight ro.Direction }
        | 'L' -> { ro with Direction = turnLeft ro.Direction }
        | _ -> failwith "") robot
