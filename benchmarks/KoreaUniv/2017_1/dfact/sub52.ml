(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b ->
  let rec product_inter = fun f a b ->
   if b = a then f a 
   else (f b) * product_inter f a (b-1) in
   product_inter f a b 


(* problem 5*)

let dfact : int -> int
= fun n -> 
  if n mod 2 = 0 then product (fun x->2*x) 1 (n/2)
  else product (fun x-> ((2*x)-1)) 1 ((n+1)/2)