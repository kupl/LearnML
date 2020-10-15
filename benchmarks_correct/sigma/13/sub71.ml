let rec sigma func a b =
  if a>b then 0
  else if a=b then func(a)
  else sigma func a (b-1)+ func(b)
