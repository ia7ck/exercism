module Tournament

let tally (input: seq<string>) =
    let results =
        input
        |> Seq.map (fun row ->
            match row.Split(';') with
            | [| team1; team2; judge |] -> (team1, team2, judge)
            | _ -> failwith "Parse Error")

    let win_count (team: string) =
        results
        |> Seq.filter (fun (t1, t2, judge) ->
            (t1 = team && judge = "win")
            || (t2 = team && judge = "loss"))
        |> Seq.length

    let loss_count (team: string) =
        results
        |> Seq.filter (fun (t1, t2, judge) ->
            (t1 = team && judge = "loss")
            || (t2 = team && judge = "win"))
        |> Seq.length

    let draw_count (team: string) =
        results
        |> Seq.filter (fun (t1, t2, judge) ->
            (t1 = team && judge = "draw")
            || (t2 = team && judge = "draw"))
        |> Seq.length

    let teams =
        results
        |> Seq.map (fun (t1, t2, _) -> [ t1; t2 ])
        |> Seq.concat
        |> Seq.sort
        |> Seq.distinct

    let results =
        teams
        |> Seq.map (fun t ->
            let win = win_count t
            let draw = draw_count t
            let loss = loss_count t
            let point = win * 3 + draw
            (t, win, draw, loss, point))
        |> Seq.sortBy (fun (t, _, _, _, point) -> (-point, t))

    let header =
        sprintf "%-30s | MP |  W |  D |  L |  P" "Team"

    let rows =
        results
        |> Seq.map (fun (t, win, draw, loss, point) ->
            sprintf "%-30s | %2d | %2d | %2d | %2d | %2d" t (win + draw + loss) win draw loss point)

    Seq.append [ header ] rows |> Seq.toList
