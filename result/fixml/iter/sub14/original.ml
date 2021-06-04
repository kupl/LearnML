let iter (n, f) =
  let rec iter_sub (m, g, f') =
    if m = 1 then g
    else if m < 1 then g
    else iter_sub (m - 1, fun x -> (f' (g x), f'))
  in
  iter_sub (n, f, f)
