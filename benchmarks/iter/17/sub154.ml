let rec iter ((n,g) : int * ('a -> 'a)) : 'a -> 'a = fun k ->
  if n > 0 then g (iter ( n-1 , g) k)
  else k