module KindergartenGarden

type Plant =
    | Grass = 0
    | Clover = 1
    | Radishes = 2
    | Violets = 3

let index1 (plant: char) =
    [ 'G'; 'C'; 'R'; 'V' ] |> List.findIndex ((=) plant)

let index2 (student: string) =
    [ "Alice"
      "Bob"
      "Charlie"
      "David"
      "Eve"
      "Fred"
      "Ginny"
      "Harriet"
      "Ileana"
      "Joseph"
      "Kincaid"
      "Larry" ] |> List.findIndex ((=) student)

let plants (diagram: string) (student: string): Plant list =
    let j = index2 student
    let d = diagram.Split("\n")
    [ (0, j * 2)
      (0, j * 2 + 1)
      (1, j * 2)
      (1, j * 2 + 1) ]
    |> List.map (fun (i, j) ->
        let k = index1 d.[i].[j]
        enum k)
