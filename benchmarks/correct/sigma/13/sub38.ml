let rec sigma f a b =
  if (a>b) then 0
  else sigma f (a+1) b + (f a)
