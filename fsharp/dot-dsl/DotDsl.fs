module DotDsl

type Elem =
    | Attr of key: string * value: string
    | Node of key: string * attrs: list<string * string>
    | Edge of left: string * right: string * attrs: list<string * string>

type Graph = Graph of elements: list<Elem>

let graph children = Graph.Graph(children)

let attr key value = Elem.Attr(key, value)

let node key attrs = Elem.Node(key, attrs)

let edge left right attrs = Elem.Edge(left, right, attrs)

// https://docs.microsoft.com/ja-jp/dotnet/fsharp/language-reference/discriminated-unions#unwrapping-discriminated-unions
let filterSort (Graph elems) pred = elems |> List.filter pred |> List.sort

let attrs graph =
    filterSort graph (fun e ->
        match e with
        | Attr _ -> true
        | _ -> false)

let nodes graph =
    filterSort graph (fun e ->
        match e with
        | Node _ -> true
        | _ -> false)

let edges graph =
    filterSort graph (fun e ->
        match e with
        | Edge _ -> true
        | _ -> false)
