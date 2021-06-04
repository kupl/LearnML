let rec iter ((n : int), (f : int -> int)) : int -> int =
  let f2 (x : int) : int =
    match n with 0 -> f x | 1 -> f x | _ -> f (iter (n - 1, f) x)
  in
  f2
