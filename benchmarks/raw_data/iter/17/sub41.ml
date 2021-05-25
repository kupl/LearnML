let rec iter (n, f) x =
  match n with
  | 0 -> x
  | n -> f (iter(n-1, f) x)


