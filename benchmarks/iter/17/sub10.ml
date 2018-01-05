(*print_endline "p3"*)

let rec iter ((n: int), (f: int->int)): (int->int) =
    match n with
    | n when n < 0 -> fun x -> x
    | 0 -> fun x -> x
    | n -> fun x -> x |> f |> iter ((n-1), f)

(*;; 10 |> iter (3, (fun x -> x + 1)) |> print_int*)
(*; print_endline ""*)
(*;; 102 |> iter (2, (fun x -> -x)) |> print_int*)
(*; print_endline ""*)
