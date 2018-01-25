(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
=fun f a b->
  if a<b 
    then (f b)*(product f a (b-1))
  else if a=b
    then f a
  else 1;;

(* problem 5*)
let dfact : int -> int 
=fun n->
  if n mod 2=0
    then product (fun k->2*k) 1 (n/2)
  else if n mod 2=1
    then product (fun k->2*k-1) 1 ((n+1)/2)
  else 0;;