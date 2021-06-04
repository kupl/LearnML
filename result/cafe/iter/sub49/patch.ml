let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with
  | 0 -> fun (__s5 : int) -> __s5
  | 1 -> f
  | _ -> fun (__s6 : int) -> iter (n - 1, f) (f __s6)


let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
