  (* problem 4*)
  let rec product : (int -> int) -> int -> int -> int
  = fun f a b -> if b = a then b
  else b * product f a (b-1)

  (*problem 5*)
  let rec dfact :int -> int
  =fun n -> if n =1 || n =2  then n
  else n * dfact (product ( fun x-> x) (n-2) (n-2))