let iter ((n : int), (f : int -> int)) : int -> int =
  let rec iter_inter ((n : int), (f : int -> int)) (x : int) : int =
    if n = 0 then x else if n = 1 then f x else f x * iter_inter (n - 1, f) x
  in
  iter_inter (n, f)
