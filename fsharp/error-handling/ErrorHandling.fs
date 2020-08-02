module ErrorHandling

let handleErrorByThrowingException () = raise (System.Exception("error"))

let handleErrorByReturningOption (input: string) =
    match System.Int32.TryParse input with
    | (true, out) -> Some out
    | (false, _) -> None

let handleErrorByReturningResult (input: string) =
    match System.Int32.TryParse input with
    | (true, out) -> Ok out
    | (false, _) -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Ok x -> switchFunction x
    | Error msg -> Error msg

let cleanupDisposablesWhenThrowingException resource =
    using resource (fun _ -> raise (System.Exception("error")))
