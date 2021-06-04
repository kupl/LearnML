let iter ((n : int), (f : 'a -> 'a)) : 'b -> 'a =
  let rec iter_sub ((m : int), (g : 'b -> 'a), (f' : 'a -> 'a)) : 'b -> 'a =
    if m = 1 then g
    else if m < 1 then g
    else iter_sub (m - 1, fun x -> (f' (g x), f'))
  in
  iter_sub (n, f, f)
