(*print_endline "p3"*)

let rec iter ((n: int), (f: 'a->'a)): ('a->'a) =
    match n with
    | n when n < 0 -> fun x -> x
    | 0 -> fun x -> x
    | n -> fun x -> x |> f |> iter ((n-1), f)

(*;; 10 |> iter (3, (fun x -> x + 1)) |> print_int*)
(*; print_endline ""*)
(*;; 102 |> iter (2, (fun x -> -x)) |> print_int*)
(*; print_endline ""*)
(*;; 10.0 |> iter (3, (fun x -> x +. 1.0)) |> print_float*)
(*; print_endline ""*)
