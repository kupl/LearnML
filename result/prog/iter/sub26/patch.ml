let iter ((n : int), (f : int -> int)) : int -> int =
  if n = 0 then fun (__s9 : int) -> __s9
  else
    let rec a (n : int) (f : int -> int) (x : int) : int =
      if n > 0 then f (a (n - 1) f x) else x
    in
    a n f
