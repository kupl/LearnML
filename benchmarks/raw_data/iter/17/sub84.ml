(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/1-3.ml *)

let rec iter ((n : int), (f : 'a -> 'a)) (input : 'a) : 'a =
  if n = 0 then input
  else f (iter (n-1, f) input)

(*
let fcn = fun x -> x ^ x

let _ = (print_string (iter (3, fcn) "Hello ")); print_newline()
*)
