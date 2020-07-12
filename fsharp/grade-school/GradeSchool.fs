module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    let students =
        match school |> Map.tryFind grade with
        | Some students -> students
        | None -> []
    school |> Map.add grade (student :: students)

let roster (school: School): string list =
    school |> Map.fold (fun acc _ v -> acc @ (v |> List.sort)) []

let grade (number: int) (school: School): string list =
    match school |> Map.tryFind number with
    | Some students -> students |> List.sort
    | None -> []
