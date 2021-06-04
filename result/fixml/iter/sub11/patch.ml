let rec iter (n, fn) x =
  if n > 1 then iter (n - 1, fn) (fn x) else if n < 1 then x else fn x
