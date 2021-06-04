let iter ((n : int), (f : 'a -> 'a)) : 'b -> 'a =
  let rec iter_sub ((m : int), (g : 'b -> 'a), (f' : 'a -> 'a)) : 'b -> 'a =
    match m with
    | 0 -> fun (__s8 : int) -> __s8
    | 1 -> g
    | _ -> iter_sub (m - 1, fun __s9 -> (g (f' __s9), f'))
  in
  iter_sub (n, f, f)
