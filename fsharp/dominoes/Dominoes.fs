module Dominoes

// 石の並び順・向きを全通り試す

let removeAt (index: int) (lst: list<'a>) =
    if index >= List.length lst then
        failwith "index must be less than list length"
    else
        let (front, back) = lst |> List.splitAt index
        (List.head back, front @ List.tail back)

let check (chain: list<int * int>) =
    match chain with
    | [] -> true
    | _ ->
        let (h, _) = List.head chain
        let (_, l) = List.last chain
        l = h && chain
                 |> List.pairwise
                 |> List.forall (fun ((_, a), (b, _)) -> a = b)

let rec canChainRec (acc: list<int * int>) (stones: list<int * int>) =
    match (acc, stones) with
    | (_, []) -> check acc
    | _ ->
        let indexes = [ 0 .. List.length stones - 1 ]
        indexes
        |> List.exists (fun i ->
            let ((a, b), rest) = removeAt i stones
            canChainRec ((a, b) :: acc) rest || canChainRec ((b, a) :: acc) rest)

let canChain input = canChainRec [] input
