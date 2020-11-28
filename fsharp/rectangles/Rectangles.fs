module Rectangles

// parse パート要る？
// 長方形の左上・右下を全通り試す

type Point =
    | Corner
    | Yoko
    | Tate

let rectangles (lines: list<string>) =
    let points =
        lines
        |> Seq.mapi (fun i row ->
            row
            |> Seq.mapi (fun j ch ->
                (i,
                 j,
                 match ch with
                 | '+' -> Some(Point.Corner)
                 | '-' -> Some(Point.Yoko)
                 | '|' -> Some(Point.Tate)
                 | _ -> None)))
        |> Seq.concat

    let is_corner (p: Point option) = p |> Option.contains Point.Corner

    let is_corner_ij ((i, j): int * int) =
        match points
              |> Seq.tryFind (fun (ii, jj, _) -> (ii, jj) = (i, j)) with
        | Some ((_, _, p)) -> is_corner p
        | None -> false

    let check_edge (edge_points: seq<int * int>) (d: Point) =
        edge_points
        |> Seq.forall (fun (i, j) ->
            points
            |> Seq.exists (fun (ii, jj, p) ->
                (ii, jj) = (i, j)
                && match p with
                   | Some (p) -> p = d || p = Point.Corner
                   | None -> false))

    let check_yoko_edge (yoko_points: seq<int * int>) = check_edge yoko_points Yoko

    let check_tate_edge (tate_points: seq<int * int>) = check_edge tate_points Tate

    let rects =
        Seq.allPairs points points
        |> Seq.filter (fun ((lt_i, lt_j, _), (rb_i, rb_j, _)) -> lt_i < rb_i && lt_j < rb_j)
        |> Seq.filter (fun ((lt_i, lt_j, p), (rb_i, rb_j, q)) ->
            let top =
                [ lt_j .. rb_j ] |> Seq.map (fun j -> (lt_i, j))

            let bottom =
                [ lt_j .. rb_j ] |> Seq.map (fun j -> (rb_i, j))

            let left =
                [ lt_i .. rb_i ] |> Seq.map (fun i -> (i, lt_j))

            let right =
                [ lt_i .. rb_i ] |> Seq.map (fun i -> (i, rb_j))

            is_corner p // 左上
            && is_corner q // 右下
            && is_corner_ij (lt_i, rb_j) // 右上
            && is_corner_ij (rb_i, lt_j) // 左下
            && check_yoko_edge top
            && check_yoko_edge bottom
            && check_tate_edge left
            && check_tate_edge right)


    rects |> Seq.length
