let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with 1 -> f | _ -> iter (n - 1, f)
