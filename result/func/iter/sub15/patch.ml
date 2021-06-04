let rec iter ((n : int), (f : int -> int)) (k : int) : int =
  match n with
  | 0 -> fun __s5 -> k n
  | _ -> f (iter (n - 1, f) k)
  | t -> iter (f n, f) (k - 1)
