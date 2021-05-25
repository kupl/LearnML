let rec iter (n, f) value =
    if n = 0 then value
  else iter (n - 1, f) (f value);;
