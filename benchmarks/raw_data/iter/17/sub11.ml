(*print_endline "p3"*)

let rec iter ((n: int), (f: 'a->'a)): ('a->'a) =
  if (n<0) then fun x -> x
  else if (n=0) then fun x -> x
  else fun x -> iter (n-1,f) (f x)

(*;; 10 |> iter (3, (fun x -> x + 1)) |> print_int*)
(*; print_endline ""*)
(*;; 102 |> iter (2, (fun x -> -x)) |> print_int*)
(*; print_endline ""*)
(*;; 10.0 |> iter (3, (fun x -> x +. 1.0)) |> print_float*)
(*; print_endline ""*)
