let rec sigma : int * int * (int -> int) -> int = fun(a, b, f) ->
  if a > b then
    0
  else if a < b then
    f(a) + sigma(a+1, b, f)
  else
    f(b)
