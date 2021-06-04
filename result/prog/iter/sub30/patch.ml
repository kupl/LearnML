let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  match n with 0 -> fun __s5 -> x f | _ -> iter (n - 1, f) (f x)
