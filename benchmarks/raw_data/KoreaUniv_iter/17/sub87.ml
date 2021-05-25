let rec iter (n, f) =
  match n with
  |0 -> fun x-> x
  |_ -> fun x -> f (iter (n-1, f) x)