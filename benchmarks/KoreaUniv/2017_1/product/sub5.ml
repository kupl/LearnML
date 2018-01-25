(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec func n = if n=b then f n
                                       else f n * (func (n+1))
  in func a