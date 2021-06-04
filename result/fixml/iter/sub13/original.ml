let rec iter : int * ('a -> 'a) -> 'a -> 'a =
 fun (n, func) x ->
  let nx = func x in

  match n with 0 -> x | _ -> iter (n - 1, func) nx
