let rec iter ((n : int), (f : int -> int)) (k : int) : int =
  match k with 0 -> f n | t -> iter (f n, f) (k - 1)
