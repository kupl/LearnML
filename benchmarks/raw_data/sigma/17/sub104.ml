(* ex 2 *)
let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
  if (a > b) then 0
  else f(a) + sigma(a+1, b, f)

(* ex 2 test *)
(*
let square x = x * x
let added = sigma (1, 5, square)
let _ = print_endline(string_of_int added)
*)
