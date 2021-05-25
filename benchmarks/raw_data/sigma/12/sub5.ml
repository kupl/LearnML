let rec sigma : int * int * (int->int) -> int = fun(a, b, f ) ->
  if b = a then f b
  else if b > a then f b + sigma (a, b-1, f)
  else 0