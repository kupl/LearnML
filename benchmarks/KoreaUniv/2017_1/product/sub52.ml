(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b ->
  let rec product_inter = fun f a b ->
   if b = a then f a 
   else (f b) * product_inter f a (b-1) in
   product_inter f a b 