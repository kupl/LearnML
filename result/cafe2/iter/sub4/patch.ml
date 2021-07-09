let rec iter ((n : int), f) : 'a -> 'a =
  match n with
  | 0 -> fun (__s5 : int) -> __s5
  | 1 -> f
  | _ -> fun (__s6 : int) -> iter (n - 1, f) (f __s6)
