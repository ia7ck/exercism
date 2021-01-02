module RobotName

open System
open System.Collections.Generic

let select (l: 'a list) =
    let rng = Random()
    List.item (rng.Next(0, l.Length)) l

let set = HashSet<string>()

let mkRobot () =
    let robot =
        Seq.initInfinite (fun _ ->
            [ List.init 2 (fun _ -> select [ 'A' .. 'Z' ])
              List.init 3 (fun _ -> select [ '0' .. '9' ]) ]
            |> List.concat
            |> String.Concat)
        |> Seq.skipWhile (fun r -> set.Contains(r))
        |> Seq.head

    set.Add(robot) |> ignore
    robot

let name robot = robot

let reset robot =
    set.Remove(robot) |> ignore
    mkRobot ()
