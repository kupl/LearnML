let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  match n with 0 -> f x | 1 -> f x | _ -> f (iter (n - 1, f) x)
