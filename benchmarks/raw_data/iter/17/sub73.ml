let rec iter : (int * ('a -> 'a)) -> 'a -> 'a = fun(n, f) i ->
  if n==0 then i
  else iter(n-1, f) (f i)
