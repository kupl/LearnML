exception Error of string

let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  if n = 0 then x
  else match n with 0 -> 0 | 1 -> f x | _ -> f (iter (n - 1, f) x)
