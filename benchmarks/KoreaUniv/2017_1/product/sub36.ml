
  (* problem 4*)
  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> if b = a then b
  else b * product f a (b-1)
