module Minesweeper

open System

let annotate (input: string list): string list =
    let board =
        input |> List.map Array.ofSeq |> Array.ofList

    let count_adjacent_mines i j =
        [ (i - 1, j)
          (i - 1, j - 1)
          (i, j - 1)
          (i + 1, j - 1)
          (i + 1, j)
          (i + 1, j + 1)
          (i, j + 1)
          (i - 1, j + 1) ]
        |> List.filter (fun (y, x) ->
            0 <= y
            && y < board.Length
            && 0 <= x
            && x < board.[y].Length
            && board.[y].[x] = '*')
        |> List.length

    let digit2char d = char (d + int '0')

    [ for i in 0 .. (board.Length - 1) ->
        [ for j in 0 .. (board.[i].Length - 1) ->
            if board.[i].[j] = '*' then
                '*'
            else
                let cnt = count_adjacent_mines i j
                if cnt = 0 then ' ' else digit2char cnt ] ]
    |> List.map String.Concat
