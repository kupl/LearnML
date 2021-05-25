let rec sigma = fun ((a : int), (b: int), (f: int -> int)) ->
  if a > b
  then 0
  else
  if a = b
  then f a
  else (f b) + sigma (a, b - 1, f)
