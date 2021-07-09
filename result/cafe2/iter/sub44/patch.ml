let rec iter ((n : int), (f : int -> int)) : int -> int =
  let f2 (x : int) : int =
    match n with 0 -> f x | 1 -> f x | _ -> f (iter (n - 1, f) x)
  in

  match n with
  | 0 -> fun (__s5 : int) -> __s5
  | __s6 -> fun (__s7 : int) -> iter (n - 1, f) (f __s7)
