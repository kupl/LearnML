let rec iter ((n : int), (func : 'a -> 'a)) x =
  let nx = func x in

  match n with 0 -> x | _ -> iter (n - 1, func) nx
