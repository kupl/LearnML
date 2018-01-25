let rec iter (n, f ) (x : int) =
  if n < 0 then raise (Failure "n could not be smaller than zero")
  else if n = 0 then x
  else f (iter (n-1, f) x);;
