(* problem 5*)

let rec dfact : int -> int = fun n ->
  match n with
  | 0 | 1 -> 1
  | _ -> n * dfact (n - 2);;