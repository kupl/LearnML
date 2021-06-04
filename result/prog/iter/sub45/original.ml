let compose (f : 'b -> 'a) (g : 'c -> 'b) (x : int) = f (g x)

let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with 0 -> f | 1 -> f | _ -> compose f (iter (n - 1, f))
