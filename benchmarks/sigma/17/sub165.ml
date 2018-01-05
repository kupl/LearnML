let rec sigma : int * int * (int -> int) -> int = fun (a, b, f) ->
  if a > b then 0
  else (f a) + (sigma ((a+1), b, f))

(* TESTING FIELD BELOW *)

let times2 x = x+x

let _ = print_endline(string_of_int(sigma (0, 10, times2)))
