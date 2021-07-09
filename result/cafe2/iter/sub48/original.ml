let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with
  | 1 -> f
  | _ -> let g : int -> int = iter (n - 1, f) in

         fun (x : int) -> f (g x)
