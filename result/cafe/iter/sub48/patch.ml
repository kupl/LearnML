let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with
  | 0 -> fun (__s5 : int) -> __s5
  | 1 -> f
  | _ -> let g : int -> int = iter (n - 1, f) in

         fun (x : int) -> f (g x)
