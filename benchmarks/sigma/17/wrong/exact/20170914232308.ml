let rec sigma : int * int * (int -> int) -> int = fun (a,b,f) ->
  if a > b then 0
  else f + sigma(a+1, b, f)
