let rec sigma (a, b, func) =
  if a>b then 0
  else if a=b then func(a)
  else sigma(a, b-1, func) + func(b)
