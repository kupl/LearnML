let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with 0 -> f | _ -> iter (n - 1, fun (f : int) -> f)
