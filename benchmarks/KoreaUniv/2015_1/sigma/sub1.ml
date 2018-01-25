(* exception *)
exception Improper_input
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> 
if a > b then raise Improper_input
else if a = b then (f b)
else (f a) + (sigma f (a + 1) b)
