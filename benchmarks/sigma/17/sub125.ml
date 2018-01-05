(* 2015-11380 박찬양 HW1-2 *)

let rec sigma: int * int * (int -> int) -> int = fun (a, b, func) ->
  if a=b then func a
  else if a > b then 0
  else func a + sigma(a+1, b, func)