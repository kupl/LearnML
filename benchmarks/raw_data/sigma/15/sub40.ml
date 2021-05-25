let rec sigma : int * int * (int -> int) -> int = fun (a, b, func) ->
  (* val sigma : int * int * (int -> int) -> int*)
  if a>b then 0 else (if a==b then (func a) else (sigma ((a+1), b, func))+(func a));;
