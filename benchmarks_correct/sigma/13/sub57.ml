let rec sigma f a b = 
  if a > b then 0 else f(b) + sigma f a (b-1);;
