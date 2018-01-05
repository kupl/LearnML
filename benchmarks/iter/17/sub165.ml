let rec iter : int * (int -> int) -> int -> int  = fun (n, f) k ->
  match k with
  | 0 -> f n
  | t -> iter (f n, f) (k-1)

(* TESTING FIELD BELOW *)

let a32 = iter (0, function x -> 2*x) 4
let a33 = iter (11, function x -> 2*x+1) 7

let _ = print_endline(string_of_int(a32))
let _ = print_endline(string_of_int(a33))
