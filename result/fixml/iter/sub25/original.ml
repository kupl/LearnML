let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) x ->
  if n = 1 then f x
  else if n < 1 then raise Failure "ERROR"
  else f (iter (n - 1, f) x)
