module RnaTranscription

open System

let toRna (dna: string): string =
    dna
    |> Seq.toList
    |> List.map (fun c ->
        match c with
        | 'G' -> 'C'
        | 'C' -> 'G'
        | 'T' -> 'A'
        | 'A' -> 'U'
        | _ -> failwith "unreachable!")
    |> List.toArray
    |> String
