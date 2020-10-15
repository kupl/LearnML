let rec sigma' (a, b, f) =
  if b < a then 0
  else (f a) + (sigma' (a+1, b, f))
let rec sigma f a b  =
  sigma' (a, b, f)
