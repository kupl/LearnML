let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) ->
  if n = 0 then fun b -> 0
  else if n = 1 then f
  else fun a -> iter (n - 1, f) (f a)
