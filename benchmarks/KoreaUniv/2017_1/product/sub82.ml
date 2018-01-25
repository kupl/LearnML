
  (* problem 4*)

  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> 
  if a<b then (product f (a+1) b)*f(a)
  else f(a)
