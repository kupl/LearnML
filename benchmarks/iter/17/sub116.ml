let rec iter (n, f) x =
  if (n<0) then raise (Failure "Negative Number")
  else if (n=0) then x
  else iter(n-1,f) (f x)
