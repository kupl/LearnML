exception Error of string

let rec iter (n, f) x =
  if n < 0 then raise Error "Illegal input"
  else match n with 0 -> x | 1 -> f x | _ -> f (iter (n - 1, f) x)
