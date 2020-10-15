let rec sigma f a b  =
  if (a == b) then (f(b))
  else (sigma f (a+1) b + f(a))
