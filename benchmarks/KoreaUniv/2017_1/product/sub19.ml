
  (* problem 4*)

  let rec  product : (int -> int) -> int -> int -> int
  = fun f a b -> let f = fun x -> x * 1 in if a> b then 1
  else (f a) * product f (a+1) b