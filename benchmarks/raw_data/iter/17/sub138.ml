let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
  if n == 0 then fun x -> x
  else if n == 1 then f
  else fun x -> (iter(n-1, f)) (f x)

