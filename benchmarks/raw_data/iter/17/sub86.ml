let rec iter ((n: int), (f : 'a -> 'a)) (param: 'a) : 'a =
  match (n, f) with
  | (0, f) -> param
  | (n, f) -> f (iter(n-1, f) param)
