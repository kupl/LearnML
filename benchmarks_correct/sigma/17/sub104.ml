(* ex 2 *)
let rec sigma f a b =
  if (a > b) then 0
  else f(a) + sigma f (a+1) b

(* ex 2 test *)
(*
let square x = x * x
let added = sigma (1, 5, square)
let _ = print_endline(string_of_int added)
*)
