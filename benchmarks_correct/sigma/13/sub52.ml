let rec sigma f a b =
  if (a > b) then 0 else
  if (a < b) then f(a)+sigma f (a+1) b
  else f(a)
